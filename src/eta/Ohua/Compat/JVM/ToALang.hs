{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Ohua.Compat.JVM.ToALang where


import           Control.Category          hiding (id, (.))
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as S
import qualified Data.Text                 as T
import           Java
import           Lens.Micro
import           Lens.Micro.Mtl
import           Lens.Micro.Internal
import           Ohua.ALang.Lang
import           Ohua.Compat.JVM.ClojureST as ClST
import           Ohua.Compat.JVM.Marshal
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util
import Ohua.DFLang.Lang (DFVar(DFEnvVar))



class ToBinding b where
    toBinding :: b -> Binding

instance ToBinding Binding where toBinding = id
instance ToBinding ClST.Symbol where toBinding = symToBinding

newtype Unevaluated a = Unevaluated { unwrapUnevaluated :: a } deriving (Functor, Show, Eq)

type Evaluator a b = Unevaluated a -> b

createEvaluator :: (a -> b) -> Evaluator a b
createEvaluator f = f . unwrapUnevaluated

letSym, letStarSym, algoSym, fnSym, fnStarSym, ifSym :: ClST.Symbol

isLetSym, isAlgoSym :: ClST.Symbol -> Bool

letSym = Symbol Nothing "let"
letStarSym = Symbol Nothing "let*"
isLetSym a
    =  a == letSym
    || a == letStarSym
fnSym = Symbol Nothing "fn"
fnStarSym = Symbol Nothing "fn*"
algoSym = Symbol Nothing "algo"
qualAlgoSym = Symbol (Just "ohua.lang") "algo"
isAlgoSym s
    =  s == algoSym
    || s == fnSym
    || s == fnStarSym
    || s == qualAlgoSym

ifSym = Symbol Nothing "if"

isIfSym = (== ifSym)


unitHE :: HostExpr
unitHE = HostExpr (-1)


unitSym :: ResolvedSymbol
unitSym = Env unitHE


unitExpr :: AExpr s ResolvedSymbol
unitExpr = Var unitSym


dfVarUnit :: DFVar
dfVarUnit = DFEnvVar unitHE


ifFunc :: QualifiedBinding
ifFunc = "ohua.lang/if"

data Registry = Registry
    { registryResolveSf :: Binding -> Maybe QualifiedBinding
    , registryResolveAlgo :: Binding -> Maybe Algo
    }

newtype DeclaredSymbols = DeclaredSymbols { unwrapDeclaredSymbols :: HS.HashSet Binding }

noDeclaredSymbols :: DeclaredSymbols
noDeclaredSymbols = DeclaredSymbols mempty

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition i l = pref:partition i rest
  where
    (pref, rest) = splitAt i l


type Field1' s a = Field1 s s a a
type Field2' s a = Field2 s s a a


isDefined :: (MonadReader r m, Field1' r DeclaredSymbols, ToBinding b) => b -> m Bool
isDefined b = asks (HS.member (toBinding b) . unwrapDeclaredSymbols . (^. _1))


isSf :: (MonadReader r m, Field2' r Registry, ToBinding b) => b -> m (Maybe QualifiedBinding)
isSf b' = (($ b) . registryResolveSf) <$> view _2
  where b = toBinding b'


isAlgo :: (MonadReader r m, Field2' r Registry, ToBinding b) => b -> m (Maybe Algo)
isAlgo b' = (($ b) . registryResolveAlgo) <$> view _2
  where b = toBinding b'


assignIds :: (Monad m, MonadGenId m) => Expression -> m Expression
assignIds e = do
    e' <- lrPostwalkExprM go e
    -- reset the id counter to the highest id currently assigned so as to avoid clashes
    -- futher in the compiler
    counterState <- generateId
    resetIdCounter $ max counterState (if HS.null ids then 0 else maximum ids)
    pure e'
  where
    ids = foldr' f mempty e
    f (Sf _ (Just id)) = HS.insert id
    f _ = id

    genNewId = do
        id <- generateId
        if id `HS.member` ids then
            generateId
        else
            pure id
    go (Var (Sf name Nothing)) = Var . Sf name . Just <$> genNewId
    go e = pure e


-- Discuss algos with no inputs with sebastian!
toALang :: Registry -> ST -> OhuaM Object (Expression, Seq (Either (Unevaluated Object) (NLazy Object)))
toALang reg st = do
    (a, s, ()) <- runRWST (go st) (noDeclaredSymbols, reg) (mempty, mempty)
    a' <- assignIds a
    pure (a', s ^. _1)
  where
    go (Literal o) = mkEnvExpr o
    go (Sym s) = do
        isLocal <- isDefined s

        if isLocal then
            Var . Local <$> expectUnqual s
        else do
            msf <- isSf s
            malgo <- isAlgo s
            case (msf, malgo) of
                (Just _, Just _) -> failWith $ "ambiguous reference" <> showT s
                (Just sf, Nothing) -> pure $ Var $ Sf sf Nothing
                (Nothing, Just algo) -> integrateAlgo s algo
                (Nothing, Nothing) -> mkEnvExpr s
    go (Vec v) = mkEnvExpr v
    go (Form []) = failWith "Empty form"
    go form@(Form (Sym sym:rest))
        | isLetSym sym = do
            when (sym == letStarSym) $
                logWarnN "let should not be expanded to `let*`. This may cause compile failures. \
                            \When macroexpanding use `ohua.util/macroexpand-all` instead."
            case rest of
                Vec v:statements -> handleLet (handleStatements statements) (partition 2 $ vectorToList v)
                _ -> failWith "Expected binding vector"
        -- NOTE: This assumes a form of `(algo [] ...)` (or `(fn [] ...)`) it currently does not handle
        -- things like `(fn ([] ...))` perhaps we should ...
        | isAlgoSym sym = do
            when (sym == fnSym || sym == fnStarSym) $
                logWarnN "DEPRECATED: The use of `fn` is deprecated, use `algo` instead."
            when (sym == fnStarSym) $
                logWarnN "fn should not be expanded to fn*. This may cause compile failures. \
                        \When macroexpanding use `ohua.util/macroexpand-all` instead."
            case rest of
                Vec v:statements -> do
                    assigns <- mapM handleAssign (vectorToList v)
                    ($) <$> mkLams assigns <*> registerBnds (assigns >>= flattenAssign) (handleStatements statements)
                _ -> failWith $ "Exprected binding vector in algo form " <> showT form
        | isIfSym sym = do
            (cond, then_, else_) <-
                case rest of
                    -- for support of no-else-statement
                    [condition, then_] ->
                        -- pure (condition, then_, Nothing)
                        failWith $ "NOT SUPPORTED: If with only two arguments is not supported \
                                   \yet. See Issue #14."
                    [condition, then_, else_] -> pure (condition, then_, Just else_)
                    _ -> failWith $ "Wrong number of arguments for `if`. Expected 3, got " <> showT (length rest)
            c <- go cond
            t <- go then_
            e <- maybe (pure $ error "IMPOSSIBLE") go else_
            pure $ Var (Sf ifFunc Nothing) `Apply` c `Apply` Lambda "_" t `Apply` Lambda "_" e
    go (Form list) = do
        (fn:rest) <- mapM go list

        -- I insert `unit` here as argument to functions with no arguments.
        -- This is remvoed again after lowering to DFLang with `Ohua.Compat.JVM.cleanUnits`.
        -- Be aware that the `unit` value itself is a hack and will break resolving
        -- env args when not properly removed!
        -- Make sure to pay special attention to the unit value and its application again when
        -- coercing env args!
        return $ foldl' (\e v -> e `Apply` v) fn (if null rest then [unitExpr] else rest)

    mkLams []   = Lambda . Direct <$> generateBindingWith "_"
    mkLams more = return $ foldl (.) id $ map Lambda more

    -- getNullExpr = Var . Env <$> getNullRef

    -- getNullRef = use _3 >>= \case
    --     Nothing -> do
    --         he <- mkEnvRef $ (superCast :: JString -> Object) $ (maybeToJava :: Maybe String -> JString) Nothing
    --         _3 .= Just he
    --         pure he
    --     Just he -> pure he

    handleLet f = go'
      where
        go' [] = f
        go' ([assign, val]:rest) = do
            assign' <- handleAssign assign
            registerAssign assign' $ Let assign' <$> go val <*> go' rest
        go' _ = failWith "Expected two element sequence"


    handleAssign (Sym s) = return $ Direct $ symToBinding s
    handleAssign (Vec v) = Destructure <$> mapM expectSym (vectorToList v)
    handleAssign _       = failWith "Invalid type of assignment"

    expectSym (Sym s) = return $ symToBinding s
    expectSym _       = failWith "Expected symbol"

    expectUnqual (Symbol Nothing name) = return $ Binding name
    expectUnqual sym = failWith $ "Expected unqualified symbol, got " <> showT sym

    handleStatements [] = failWith "Expected at least one return form in"
    handleStatements [x] = go x
    handleStatements (stmt:rest) = (Let . Direct <$> generateBindingWith "_") <*> go stmt <*> handleStatements rest

    mkEnvExpr :: (ToEnvExpr e, MonadReadEnvExpr m, MonadState s m, Field1' s (Seq (Either (Unevaluated Object) (NLazy Object))), MonadGenBnd m, MonadRecordEnvExpr m, EnvExpr m ~ Object) => e -> m Expression
    mkEnvExpr = fmap (Var . Env) . mkEnvRef

    mkEnvRef thing = do
        i <- S.length <$> use _1
        _1 %= (|> Left (Unevaluated $ toEnvExpr thing))
        pure $ HostExpr i

    registerBnds bnds = local (_1 %~ DeclaredSymbols . HS.union (HS.fromList bnds) . unwrapDeclaredSymbols)
    registerAssign assign = registerBnds $ flattenAssign assign


integrateAlgo :: (MonadState s m, Field1' s (Seq (Either (Unevaluated Object) (NLazy Object))), Field2' s (HM.HashMap ClST.Symbol Expression)) => ClST.Symbol -> Algo -> m Expression
integrateAlgo aname (Algo code envExprs) = do
    cached <- gets (^? _2 . ix aname)
    case cached of
        Just a -> pure a
        Nothing -> do
            i <- S.length <$> use _1
            _1 %= (<> fmap Right envExprs)
            let adjusted = shiftEnvExprs i code
            _2 . at aname .= Just adjusted
            pure adjusted


shiftEnvExprs :: Int -> Expression -> Expression
shiftEnvExprs offset = lrMapRefs $ \case
    Env (HostExpr i) -> Env $ HostExpr $ i + offset
    a -> a


bndToFnName :: MonadOhua env m => Binding -> m QualifiedBinding
bndToFnName (Binding b) =
    case symbolFromString b of
        Left err -> failWith $ T.pack err
        Right (Unqual _) -> failWith $ "Could not convert " <> showT b <> " to function name"
        Right (Qual fnName) -> return fnName


symToBinding :: ClST.Symbol -> Binding
symToBinding = Binding . \case
    Symbol Nothing name -> name
    Symbol (Just ns) name -> ns <> "/" <> name


definedBindings :: ST -> HS.HashSet Binding
definedBindings = execWriter . go
  where
    go (Sym s)          = tell $ HS.singleton $ symToBinding s
    go (Form exprs)     = mapM_ go exprs
    go (Vec (Vector v)) = mapM_ go v
    go _                = return ()

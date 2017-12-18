{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
module Ohua.Compat.JVM.ToALang where


import qualified Clojure
import qualified Clojure.Core as Clojure
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
import           Lens.Micro.Internal
import           Lens.Micro.Mtl
import           Ohua.ALang.Lang
import           Ohua.Compat.JVM.ClojureST as ClST
import           Ohua.Compat.JVM.Marshal
import           Ohua.DFLang.Lang          (DFVar (DFEnvVar))
import           Ohua.LensClasses
import           Ohua.Monad hiding (getEnvExpr)
import           Ohua.Types
import           Ohua.Util
import qualified Ohua.Util.Str as Str



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
    { registryResolveSf   :: Binding -> Maybe QualifiedBinding
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
    f _                = id

    genNewId = do
        id <- generateId
        if id `HS.member` ids then
            generateId
        else
            pure id
    go (Var (Sf name Nothing)) = Var . Sf name . Just <$> genNewId
    go e                       = pure e


initExprKw :: Object
initExprKw = Clojure.keyword "init-state"


getCljSfnInitExpr :: (MonadError Error m, MonadState s m, Field1' s EnvExprs) 
                  => AnnotatedST Object -> m (Maybe Object)
getCljSfnInitExpr st = pure $ Clojure.get (st ^. annotation) initExprKw


expectUnqual :: MonadError Error m => ClST.Symbol -> m Binding
expectUnqual (Symbol Nothing name) = pure $ Binding name
expectUnqual sym = failWith $ "Expected unqualified symbol, got " <> Str.showS sym


expectSym :: (HasValue thing (GenericST a), MonadError Error m, Show a) => Str.Str -> thing -> m Binding
expectSym location wAnn = case wAnn ^. value of
    Sym s -> pure $ symToBinding s
    thing -> failWith $ "Expected symbol in " <> location <> ", found " <> Str.showS thing


mkLams :: (Applicative m, MonadGenBnd m) => [Assignment] -> m (Expr a -> Expr a)
mkLams []   = Lambda . Direct <$> generateBindingWith "_"
mkLams more = pure $ foldl (.) id $ map Lambda more


handleAssign :: (Applicative m, MonadError Error m, Show a)
             => AnnotatedST a -> m Assignment
handleAssign wAnn = case wAnn ^. value of
    Sym s -> pure $ Direct $ symToBinding s
    Vec v -> Destructure <$> traverse (expectSym "assignment") (vectorToList v)
    _     -> failWith "Invalid type of assignment"


registerBnds :: (MonadReader e m, Field1' e DeclaredSymbols) => [Binding] -> m a -> m a
registerBnds bnds = local (_1 %~ DeclaredSymbols . HS.union (HS.fromList bnds) . unwrapDeclaredSymbols)


registerAssign :: (MonadReader e m, Field1' e DeclaredSymbols) => Assignment -> m a -> m a
registerAssign assign = registerBnds $ flattenAssign assign


type EnvExprs = Seq (Either (Unevaluated Object) (NLazy Object))
type AlgoMap = HM.HashMap ClST.Symbol Expression
type ToALangM = RWST (DeclaredSymbols, Registry) () (EnvExprs, AlgoMap) (OhuaM Object)


mkEnvExpr :: ToEnvExpr e => e -> ToALangM Expression
mkEnvExpr = fmap (Var . Env) . mkEnvRef

mkEnvRef :: (MonadState s m, Field1' s EnvExprs, ToEnvExpr e) => e -> m HostExpr
mkEnvRef thing = do
    i <- S.length <$> use _1
    _1 %= (|> Left (Unevaluated $ toEnvExpr thing))
    pure $ HostExpr i

getEnvExpr :: (MonadError Error m, MonadState s m, Field1' s EnvExprs) 
           => HostExpr -> m (Either (Unevaluated Object) (NLazy Object))
getEnvExpr (HostExpr index) = preuse (_1 . ix index) >>= maybe (throwError msg) pure
  where 
    msg = "Invariant broken, no env expression with index " <> Str.showS index

-- Discuss algos with no inputs with sebastian!
toALang :: Registry -> AnnotatedST Object -> OhuaM Object (Expression, EnvExprs)
toALang reg st = do
    (a, s, ()) <- runRWST (toAlang' st) (noDeclaredSymbols, reg) (mempty, mempty)
    a' <- assignIds a
    pure (a', s ^. _1)


logEnvValue :: (MonadError Error m, MonadState s m, Field1' s EnvExprs, MonadLogger m) 
            => HostExpr -> m ()
logEnvValue he = do
    o <- getEnvExpr he
    exprObj <- case o of
        Left (Unevaluated o) -> pure o
        Right o -> pure $ (superCast :: NLazy a -> Object) o
    
    logDebugN $ "Env expr [" <> showT he <> "]: " <> showT exprObj <> " : " <> showT (Clojure.type_ exprObj)


toAlang' :: AnnotatedST Object -> ToALangM Expression
toAlang' stWithAnn = case stWithAnn ^. value of
    Literal o -> mkEnvExpr o
    Sym s -> do
        isLocal <- isDefined s

        if isLocal then
            Var . Local <$> expectUnqual s
        else do
            msf <- isSf s
            malgo <- isAlgo s
            case (msf, malgo) of
                (Just _, Just _) -> failWith $ "ambiguous reference" <> Str.showS s
                (Just sf, Nothing) -> pure $ Var $ Sf sf Nothing
                (Nothing, Just algo) -> integrateAlgo s algo
                (Nothing, Nothing) -> mkEnvExpr s
    Vec v -> mkEnvExpr v
    Form [] -> failWith "Empty form"
    Form (AnnotatedST (Annotated _ (Sym sym)):rest)
        | isLetSym sym -> do
            when (sym == letStarSym) $
                logWarnN "let should not be expanded to `let*`. This may cause compile failures. \
                            \When macroexpanding use `ohua.util/macroexpand-all` instead."
            case rest of
                (AnnotatedST (Annotated _ (Vec v))):statements ->
                    handleLet (handleStatements statements) (partition 2 $ vectorToList v)
                _ -> failWith "Expected binding vector"
        -- NOTE: This assumes a form of `(algo [] ...)` (or `(fn [] ...)`) it currently does not handle
        -- things like `(fn ([] ...))` perhaps we should ...
        | isAlgoSym sym -> do
            when (sym == fnSym || sym == fnStarSym) $
                logWarnN "DEPRECATED: The use of `fn` is deprecated, use `algo` instead."
            when (sym == fnStarSym) $
                logWarnN "fn should not be expanded to fn*. This may cause compile failures. \
                        \When macroexpanding use `ohua.util/macroexpand-all` instead."
            case rest of
                (AnnotatedST (Annotated _ (Vec v))):statements -> do
                    assigns <- mapM handleAssign (vectorToList v)
                    ($) <$> mkLams assigns <*> registerBnds (assigns >>= flattenAssign) (handleStatements statements)
                _ -> failWith $ "Exprected binding vector in algo form " <> Str.showS (stWithAnn ^. value)
        | isIfSym sym -> do
            (cond, then_, else_) <-
                case rest of
                    -- for support of no-else-statement
                    [condition, then_] ->
                        -- pure (condition, then_, Nothing)
                        failWith $ "NOT SUPPORTED: If with only two arguments is not supported \
                                \yet. See Issue #14."
                    [condition, then_, else_] -> pure (condition, then_, Just else_)
                    _ -> failWith $ "Wrong number of arguments for `if`. Expected 3, got " <> Str.showS (length rest)
            c <- toAlang' cond
            t <- toAlang' then_
            e <- maybe (pure $ error "IMPOSSIBLE") toAlang' else_
            pure $
                Var (Sf ifFunc Nothing)
                    `Apply` c
                    `Apply` Lambda "_" t
                    `Apply` Lambda "_" e
    Form list@(head:_) -> do
            (fn:rest) <- mapM toAlang' list

            -- I insert `unit` here as argument to functions with no arguments.
            -- This is remvoed again after lowering to DFLang with `Ohua.Compat.JVM.cleanUnits`.
            -- Be aware that the `unit` value itself is a hack and will break resolving
            -- env args when not properly removed!
            -- Make sure to pay special attention to the unit value and its application again when
            -- coercing env args!
            logDebugN $ "Meta info: " <> showT (head ^. annotation)
            case fn of
                (Var (Env e)) -> logEnvValue e
                _ -> pure ()
            restWInit <- getCljSfnInitExpr head >>= \case
                Nothing -> pure rest
                Just o -> do
                    initExpr <- mkEnvExpr o
                    pure $ initExpr : rest

            pure $ foldl' (\e v -> e `Apply` v) fn (insertUnitExpr restWInit)
      where
        insertUnitExpr [] = [unitExpr]
        insertUnitExpr xs = xs

-- getNullExpr = Var . Env <$> getNullRef

-- getNullRef = use _3 >>= \case
--     Nothing -> do
--         he <- mkEnvRef $ (superCast :: JString -> Object) $ (maybeToJava :: Maybe String -> JString) Nothing
--         _3 .= Just he
--         pure he
--     Just he -> pure he


handleLet :: ToALangM Expression -> [[AnnotatedST Object]] -> ToALangM Expression
handleLet f = go'
    where
    go' [] = f
    go' ([assign, val]:rest) = do
        assign' <- handleAssign assign
        registerAssign assign' $ Let assign' <$> toAlang' val <*> go' rest
    go' _ = failWith "Expected two element sequence"


handleStatements :: [AnnotatedST Object] -> ToALangM Expression
handleStatements [] = failWith "Expected at least one return form in"
handleStatements [x] = toAlang' x
handleStatements (stmt:rest) = (Let . Direct <$> generateBindingWith "_") <*> toAlang' stmt <*> handleStatements rest


integrateAlgo :: ClST.Symbol -> Algo -> ToALangM Expression
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
    case symbolFromString $ Str.toString b of
        Left err -> failWith $ Str.fromString err
        Right (Unqual _) -> failWith $ "Could not convert " <> Str.showS b <> " to function name"
        Right (Qual fnName) -> return fnName


symToBinding :: ClST.Symbol -> Binding
symToBinding = Binding . \case
    Symbol Nothing name -> name
    Symbol (Just ns) name -> ns <> "/" <> name


definedBindings :: AnnotatedST a -> HS.HashSet Binding
definedBindings = execWriter . go0
  where
    go0 = go . (^. value)
    go (Sym s)          = tell $ HS.singleton $ symToBinding s
    go (Form exprs)     = mapM_ go0 exprs
    go (Vec (Vector v)) = mapM_ go0 v
    go _                = return ()

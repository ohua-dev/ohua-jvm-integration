{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ohua.Compat.JVM.ToALang where


import qualified Clojure
import qualified Clojure.Core                 as Clojure
import           Control.Arrow                (first)
import           Control.Category             hiding (id, (.))
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Trans.Cofree (CofreeF, tailF)
import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.Writer
import           Data.Foldable                as F
import           Data.Functor.Classes         (Show1)
import           Data.Functor.Compose
import           Data.Functor.Foldable
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Data.Sequence                (Seq, (|>))
import qualified Data.Sequence                as S
import qualified Data.Text                    as T
import           Java
import           Lens.Micro
import           Lens.Micro.Internal
import           Lens.Micro.Mtl
import           Ohua.ALang.Lang              as AL
import qualified Ohua.ALang.Refs
import           Ohua.Compat.JVM.ClojureST    as ClST
import           Ohua.Compat.JVM.Marshal
import           Ohua.DFLang.Lang             (DFVar (DFEnvVar))
import           Ohua.LensClasses
import           Ohua.Monad                   hiding (getEnvExpr)
import           Ohua.Types
import           Ohua.Unit
import           Ohua.Util
import qualified Ohua.Util.Str                as Str



class ToBinding b where
    toBinding :: b -> Binding

instance ToBinding Binding where toBinding = id
instance ToBinding ClST.Symbol where toBinding = symToBinding

newtype Unevaluated a = Unevaluated { unwrapUnevaluated :: a } deriving (Functor, Show, Eq)

type Evaluator a b = Unevaluated a -> b

createEvaluator :: (a -> b) -> Evaluator a b
createEvaluator f = f . unwrapUnevaluated

letSym, letStarSym, algoSym, fnSym, fnStarSym, ifSym, qualAlgoSym :: ClST.Symbol

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

ifFunc :: QualifiedBinding
ifFunc = Ohua.ALang.Refs.ifThenElse

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


getCljSfnInitExpr :: Annotated Object a -> Maybe Object
getCljSfnInitExpr st = Clojure.get (st ^. annotation) initExprKw


expectUnqual :: MonadError Error m => ClST.Symbol -> m Binding
expectUnqual (Symbol Nothing name) = pure $ Binding name
expectUnqual sym = failWith $ "Expected unqualified symbol, got " <> Str.showS sym


expectSym :: (MonadError Error m, Show a) => Str.Str -> ClST.Expr a -> m Binding
expectSym _ (Sym s) = pure $ symToBinding s
expectSym location thing = failWith $ "Expected symbol in " <> location <> ", found " <> Str.showS thing


mkLams :: (Applicative m, MonadGenBnd m) => [Assignment] -> m (AL.Expr a -> AL.Expr a)
mkLams []   = Lambda . Direct <$> generateBindingWith "_"
mkLams more = pure $ foldl (.) id $ map Lambda more


handleAssign :: (Applicative m, MonadError Error m)
             => ClST.ST -> m Assignment
handleAssign = project >>> \case
  Sym s -> pure $ Direct $ symToBinding s
  Vec v -> Destructure <$> traverse (expectSym "assignment" . project) v
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
toALang :: Registry -> AnnST Object -> OhuaM Object (Expression, EnvExprs)
toALang reg st = do
    (a, s, ()) <- runRWST (histo toAlangWorker st) (noDeclaredSymbols, reg) (mempty, mempty)
    a' <- assignIds a
    pure (a', s ^. _1)


logEnvValue :: (MonadError Error m, MonadState s m, Field1' s EnvExprs, MonadLogger m)
            => HostExpr -> m ()
logEnvValue he = do
    o <- getEnvExpr he
    exprObj <- case o of
        Left (Unevaluated o) -> pure o
        Right o              -> pure $ (superCast :: NLazy a -> Object) o

    logDebugN $ "Env expr [" <> showT he <> "]: " <> showT exprObj <> " : " <> showT (Clojure.type_ exprObj)


filteri :: (Int -> Bool) -> [a] -> [a]
filteri p = map snd . filter (p . fst) . zip [0..]


toAlangWorker :: Base (AnnST Object) (Cofree (Base (AnnST Object)) (ToALangM Expression)) -> ToALangM Expression
toAlangWorker stWithAnn@(Compose (Annotated ann val)) =
  case val of
    Lit o -> mkEnvExpr o
    Sym s -> do
      isLocal <- isDefined s

      if isLocal
        then Var . Local <$> expectUnqual s
        else do
          msf <- isSf s
          malgo <- isAlgo s
          case (msf, malgo) of
            (Just _, Just _) -> failWith $ "ambiguous reference" <> Str.showS s
            (Just sf, Nothing) -> pure $ Var $ Sf sf Nothing
            (Nothing, Just algo) -> integrateAlgo s algo
            (Nothing, Nothing) -> mkEnvExpr asST
    Vec v -> mkEnvExpr asST
    Form [] -> failWith "Empty form"
    Form (head:rest) ->
      let values = map extract rest
          trees = map unwrap rest
          annHead@(Annotated headAnn headVal) = getCompose $ unwrap head
      in
        case headVal of
          Sym sym
            | isLetSym sym -> do
                when (sym == letStarSym) $
                  logWarnN "let should not be expanded to `let*`. This may cause compile failures. \
                           \When macroexpanding use `ohua.util/macroexpand-all` instead."
                case trees of
                  AnnVec _ v:_ -> do
                    let assigns = map (recast toST) $ filteri odd v
                    assignedValues <- traverse extract $ filteri even v
                    vs <- sequence $ tail values
                    handleLet (handleStatements vs) $ zip assigns assignedValues
                  _ -> failWith "Expected binding vector"
              -- NOTE: This assumes a form of `(algo [] ...)` (or `(fn [] ...)`) it currently does not handle
              -- things like `(fn ([] ...))` perhaps we should ...
            | isAlgoSym sym -> do
                when (sym == fnSym || sym == fnStarSym) $
                  logWarnN "DEPRECATED: The use of `fn` is deprecated, use `algo` instead."
                when (sym == fnStarSym) $
                  logWarnN "fn should not be expanded to fn*. This may cause compile failures. \
                           \When macroexpanding use `ohua.util/macroexpand-all` instead."
                case trees of
                  AnnVec _ v:_ -> do
                    vs <- sequence $ tail values
                    assigns <- traverse (handleAssign . recast toST) v
                    ($) <$> mkLams assigns <*> registerBnds (assigns >>= flattenAssign) (handleStatements vs)
                  _ -> failWith "Exprected binding vector in algo form"
            | isIfSym sym -> do
                (c, t, e) <-
                  case values of
                    -- for support of no-else-statement
                    [condition, then_] ->
                      -- pure (condition, then_, Nothing)
                      failWith $ "NOT SUPPORTED: If with only two arguments is not supported \
                                 \yet. See Issue #14."
                    [condition, then_, else_] -> (,,) <$> condition <*> then_ <*> else_
                    _ -> failWith $ "Wrong number of arguments for `if`. Expected 3, got " <> Str.showS (length rest)
                pure $
                  Var (Sf ifFunc Nothing)
                    `Apply` c
                    `Apply` Lambda "_" t
                    `Apply` Lambda "_" e
          _ -> do
            (fn:args) <- sequence values

             -- I insert `unit` here as argument to functions with no arguments.
             -- This is removed again after lowering to DFLang with `Ohua.Compat.JVM.cleanUnits`.
             -- Be aware that the `unit` value itself is a hack and will break resolving
             -- env args when not properly removed!
             -- Make sure to pay special attention to the unit value and its application again when
             -- coercing env args!
            logDebugN $ "Meta info: " <> showT headAnn
            case fn of
              Var (Env e) -> logEnvValue e
              _           -> pure ()
            restWInit <- case getCljSfnInitExpr annHead of
              Nothing -> pure args
              Just o -> do
                initExpr <- mkEnvExpr o
                pure $ initExpr : args

            pure $ foldl' (\e v -> e `Apply` v) fn (insertUnitExpr restWInit)
            where
              insertUnitExpr [] = [unitExpr]
              insertUnitExpr xs = xs
  where
    asST = embed $ fmap (recast id) stWithAnn :: AnnST Object
    recast :: (Recursive t1, Corecursive a1, Base t1 ~ CofreeF f1 a2)
           => (f1 a1 -> Base a1 a1) -> t1 -> a1
    recast f = cata (embed . f . tailF)
    toST = (^. value) . getCompose
-- getNullExpr = Var . Env <$> getNullRef

-- getNullRef = use _3 >>= \case
--     Nothing -> do
--         he <- mkEnvRef $ (superCast :: JString -> Object) $ (maybeToJava :: Maybe String -> JString) Nothing
--         _3 .= Just he
--         pure he
--     Just he -> pure he


handleLet :: ToALangM Expression -> [(ClST.ST, Expression)] -> ToALangM Expression
handleLet f = cata $ \case
  Nil -> f
  Cons (assign, val) b -> do
    assign' <- handleAssign assign
    registerAssign assign' $ Let assign' val <$> b


handleStatements :: [Expression] -> ToALangM Expression
handleStatements = para $ \case
  Nil -> failWith "Expected at least one return form"
  Cons rest ([], _) -> pure rest
  Cons stmt (_, rest) -> do
    bnd <- generateBindingWith "_"
    Let (Direct bnd) stmt <$> rest


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


definedBindings :: AnnST a -> HS.HashSet Binding
definedBindings = cata $ \(Compose (Annotated _ val)) ->
  case val of
    Sym s   -> HS.singleton $ symToBinding s
    Form fs -> F.fold fs
    Vec v   -> F.fold v
    _       -> mempty

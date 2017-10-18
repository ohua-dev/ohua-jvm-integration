{-# LANGUAGE ConstraintKinds #-}
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



class ToBinding b where
    toBinding :: b -> Binding

instance ToBinding Binding where toBinding = id
instance ToBinding ClST.Symbol where toBinding = symToBinding

letSym = Symbol Nothing "let"
letStarSym = Symbol Nothing "let*"
isLetSym a = a == letSym || a == letStarSym
fnSym = Symbol Nothing "fn"


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


toALang :: MonadOhua env m => Registry -> ST -> m (Expression, Seq Object)
toALang reg st = (\(a, (s, _), ()) -> (a, s)) <$> runRWST (go st) (noDeclaredSymbols, reg) (mempty, mempty)
  where
    go (Literal o) = Var <$> toEnvRef o
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
                (Nothing, Nothing) -> Var <$> toEnvRef s
    go (Vec v) = Var <$> toEnvRef v
    go (Form []) = failWith "Empty form"
    go (Form (Sym l:rest)) | isLetSym l =
        case rest of
            Vec v:statements -> handleLet (handleStatements statements) (partition 2 $ vectorToList v)
            _ -> failWith "Expected binding vector"
    go (Form (Sym fn:rest)) | fn == fnSym =
        case rest of
            Vec v:statements -> do
                assigns <- mapM handleAssign (vectorToList v)
                ($) <$> mkLams assigns <*> registerBnds (assigns >>= flattenAssign) (handleStatements statements)
            _ -> failWith "Exprected binding vector"
    go (Form list) = do
        (fn:rest) <- mapM go list
        return $ foldl' (\e v -> e `Apply` v) fn rest

    mkLams []   = Lambda . Direct <$> generateBindingWith "_"
    mkLams more = return $ foldl (.) id $ map Lambda more

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
    handleStatements (stmt:rest) = Let <$> (Direct <$> generateBindingWith "_") <*> go stmt <*> handleStatements rest

    toEnvRef thing = do
        i <- S.length <$> use _1
        _1 %= (|> (toEnvExpr thing :: Object))
        pure $ Env $ HostExpr i

    registerBnds bnds = local (_1 %~ DeclaredSymbols . HS.union (HS.fromList bnds) . unwrapDeclaredSymbols)
    registerAssign assign = registerBnds $ flattenAssign assign


integrateAlgo :: (MonadState s m, Field1' s (S.Seq Object), Field2' s (HM.HashMap ClST.Symbol Expression)) => ClST.Symbol -> Algo -> m Expression
integrateAlgo aname (Algo code envExprs) = do
    cached <- gets (^? _2 . ix aname)
    case cached of
        Just a -> pure a
        Nothing -> do
            i <- S.length <$> use _1 
            _1 %= (<> envExprs)
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

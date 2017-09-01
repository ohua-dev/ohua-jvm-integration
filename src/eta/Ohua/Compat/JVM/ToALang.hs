module Ohua.Compat.JVM.ToALang where


import Ohua.Compat.JVM.ClojureST
import Ohua.Compat.JVM.Marshal
import Ohua.ALang.Lang
import Ohua.Monad
import qualified Data.Sequence as S
import Data.Sequence ((|>), Seq)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except
import Ohua.Types
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Foldable
import Java
import Lens.Micro
import Control.Category hiding ((.), id)
import Ohua.Util
import qualified Data.Text as T


class HasDeclaredSymbols s where
    declaredSymbols :: Lens' s DeclaredSymbols


instance HasDeclaredSymbols (DeclaredSymbols, b, c) where
    declaredSymbols = _1

class HasSfRegistry s where
    sfRegistry :: Lens' s SfRegistry

instance HasSfRegistry (a, SfRegistry, c) where
    sfRegistry = _2

class ToBinding b where
    toBinding :: b -> Binding

instance ToBinding Binding where toBinding = id
instance ToBinding Symbol where toBinding = symToBinding

letSym = Symbol Nothing "let"
fnSym = Symbol Nothing "fn"


data SfRegistry = SfRegistry 
    { sfRegistryQual :: Binding -> Bool
    , sfRegistryUnqual :: Binding -> Maybe Binding
    }

newtype DeclaredSymbols = DeclaredSymbols { unwrapDeclaredSymbols :: HS.HashSet Binding }

noDeclaredSymbols :: DeclaredSymbols
noDeclaredSymbols = DeclaredSymbols mempty

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition i l = pref:partition i rest
  where
    (pref, rest) = splitAt i l


isDefined :: (MonadReader r m, HasDeclaredSymbols r, ToBinding b) => b -> m Bool
isDefined b = asks (HS.member (toBinding b) . unwrapDeclaredSymbols . (^. declaredSymbols))


isSf :: (MonadReader r m, HasSfRegistry r, ToBinding b) => b -> m (Maybe Binding)
isSf b' = asks $ (^. sfRegistry) >>> \(SfRegistry qual unqual) -> 
    unqual b `mplus` if qual b then Just b else Nothing
  where b = toBinding b'


toALang :: MonadOhua m => SfRegistry -> ST -> m (Expression, Seq Object)
toALang reg st = (\(a, s, ()) -> (a, s)) <$> runRWST (go st) (noDeclaredSymbols, reg, ()) mempty
  where
    go (Literal o) = Var <$> toEnvRef o
    go (Sym s) = do
        isLocal <- isDefined s
        Var <$> 
            if isLocal then 
                return (Local $ symToBinding s) 
            else 
                isSf s >>= maybe (toEnvRef s) (fmap (`Sf` Nothing) . bndToFnName)
    go (Vec v) = Var <$> toEnvRef v
    go (Form []) = failWith "Empty form"
    go (Form (Sym l:rest)) | l == letSym =
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
        
    mkLams [] = Lambda . Direct <$> generateBindingWith "_"
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
    handleAssign _ = failWith "Invalid type of assignment"
    
    expectSym (Sym s) = return $ symToBinding s
    expectSym _ = failWith "Expected symbol"

    handleStatements [] = failWith "Expected at least one return form in"
    handleStatements [x] = go x
    handleStatements (stmt:rest) = Let <$> (Direct <$> generateBindingWith "_") <*> go stmt <*> handleStatements rest

    toEnvRef thing = do 
        i <- gets S.length
        modify (|> (toEnvExpr thing :: Object))
        return $ Env $ HostExpr i

    registerBnds bnds = local (declaredSymbols %~ DeclaredSymbols . HS.union (HS.fromList bnds) . unwrapDeclaredSymbols)
    registerAssign assign = registerBnds $ flattenAssign assign




bndToFnName :: MonadOhua m => Binding -> m FnName
bndToFnName (Binding b) = 
    case symbolFromString b of
        Left err -> failWith $ T.pack err
        Right (Right _) -> failWith $ "Could not convert " <> showT b <> " to function name"
        Right (Left fnName) -> return fnName


symToBinding :: Symbol -> Binding
symToBinding = Binding . \case 
    Symbol Nothing name -> name
    Symbol (Just ns) name -> ns <> "/" <> name


definedBindings :: ST -> HS.HashSet Binding
definedBindings = execWriter . go
  where
    go (Sym s) = tell $ HS.singleton $ symToBinding s
    go (Form exprs) = mapM_ go exprs
    go (Vec (Vector v)) = mapM_ go v
    go _ = return ()

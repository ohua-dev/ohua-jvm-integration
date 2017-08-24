module Ohua.Compat.JVM.ToALang where


import Ohua.Compat.JVM.ClojureST
import Ohua.Compat.JVM.Marshal
import Ohua.ALang.Lang
import Ohua.Monad
import qualified Data.Sequence as S
import Data.Sequence ((|>), Seq)
import qualified Data.HashSet as HS
import Control.Monad.Except
import Ohua.Types
import Control.Monad.RWS
import Java


letSym = Symbol Nothing "let"
fnSym = Symbol Nothing "fn"


partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition i l = pref:partition i rest
  where
    (pref, rest) = splitAt i l


toAlang :: (MonadError String m, MonadOhua m) => ST -> m (Expression, Seq Object)
toAlang = fmap (\(a, b, _) -> (a, b)) . runRWST mempty mempty . go
  where
    go (Literal o) = Var <$> toEnvRef o
    go (Sym s) = do
        isLocal <- asks (HS.member $ toBinding s)
        Var <$> if isLocal then return (Local $ toBinding s) else toEnvRef s
    go (Vec v) = Var <$> toEnvRef v
    go (Form []) = throwError "Empty form"
    go (Form (Sym l:rest)) | l == letSym =
        case rest of
            Vec v:statements -> handleLet (handleStatements statements) (partition 2 $ vectorToList v)
            _ -> throwError "Expected binding vector"
    go (Form (Sym fn:rest)) | fn == fnSym =
        case rest of
            Vec v:statements -> do 
                assigns <- mapM handleAssign (vectorToList v)
                ($) <$> mkLams assigns <*> local (HS.union $ HS.fromList $ assigns >>= flattenAssign) (handleStatements statements)
            _ -> throwError "Exprected binding vector"
        
    mkLams [] = Lambda . Direct <$> generateBindingWith "_"
    mkLams more = return $ foldl (.) id $ map Lambda more

    handleLet f = go'
      where
        go' [] = f
        go' ([assign, val]:rest) = do
            assign' <- handleAssign assign
            registerAssign assign' $ Let assign' <$> go val <*> go' rest
        go' _ = throwError "Expected two element sequence"


    handleAssign (Sym s) = return $ Direct $ toBinding s
    handleAssign (Vec v) = Destructure <$> mapM expectSym (vectorToList v)
    handleAssign _ = throwError "Invalid type of assignment"
    
    expectSym (Sym s) = return $ toBinding s
    expectSym _ = throwError "Expected symbol"

    handleStatements [] = throwError "Expected at least one return form in"
    handleStatements [x] = go x
    handleStatements (stmt:rest) = Let <$> (Direct <$> generateBindingWith "_") <*> go stmt <*> handleStatements rest

    toBinding = Binding . name

    toEnvRef thing = do 
        i <- gets S.length
        modify (|> toEnvExpr thing)
        return $ Env $ HostExpr i

    registerAssign assign = local (HS.union $ HS.fromList $ flattenAssign assign)

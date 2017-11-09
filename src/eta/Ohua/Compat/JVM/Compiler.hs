{-# LANGUAGE MagicHash #-}
module Ohua.Compat.JVM.Compiler where


import Java
import Ohua.Compile
import Ohua.Monad
import Ohua.Compat.JVM.ToALang
import Ohua.Compat.JVM.Marshal
import Data.Foldable
import Ohua.Types
import qualified Data.Text as T
import Data.Functor.Identity
import Ohua.DFGraph
import Data.Sequence as Seq
import Ohua.ALang.Lang
import Control.DeepSeq
import Ohua.Util
import Ohua.Compat.JVM.ClojureST
import qualified Ohua.Compat.JVM.Refs as JVMRefs
import Ohua.DFGraph.Show
import Ohua.DFLang.Lang
import Data.Default
import Lens.Micro



data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class

data {-# CLASS "ohua.Linker" #-} IsLinker = IsLinker (Object# IsLinker) deriving Class

foreign import java unsafe "@interface resolve" linkerResolveUnqualified :: String -> Java IsLinker (Maybe JString)
foreign import java unsafe "@interface resolveAlgo" linkerResolveAlgo :: String -> Java IsLinker (Maybe NAlgo)
foreign import java unsafe "@interface eval" linkerEval :: Object -> Java IsLinker (NLazy Object)

opts :: Options
opts = def
    & callEnvExpr .~ (Just JVMRefs.__callClojureFn)

forceA :: (NFData a, Applicative m) => a -> m ()
forceA = (`deepseq` pure ())

cleanUnits :: Applicative m => DFExpr -> m DFExpr
cleanUnits (DFExpr lets ret) = pure $ DFExpr (fmap f lets) ret
  where
    f e@(LetExpr{callArguments=[a]}) | a == dfVarUnit = e {callArguments = []}
    f e = e


basicCompile :: IsLinker -> Object -> IO (OutGraph, Seq (Either (Unevaluated Object) (NLazy Object)))
basicCompile linker thing = do
    forceAndReport "ST was valid" st
    (graph, envExprs) <- runM $ do
        (alang, envExprs) <- toALang (mkRegistry linker) st
        forceAndReport "alang converted" alang
        liftIO $ print alang
        -- liftIO $ writeFile "alang-dump" $ show alang
        graph <- pipeline (noCustomPasses :: CustomPasses (OhuaT Object IO)) {passAfterDFLowering = cleanUnits} alang
        forceAndReport "graph created" graph
        pure (graph, envExprs)
    forceAndReport "Compilation done" graph
    printAsTable $ graph
    pure (graph, envExprs)
  where
    st = fromNative thing
    runM ac = fmap (either (error . T.unpack) id) $ runOhuaT0IO opts ac (definedBindings st)


evalExprs :: IsLinker -> Seq (Either (Unevaluated Object) (NLazy Object)) -> IO (Seq (NLazy Object))
evalExprs linker = mapM $ either (javaWith linker . linkerEval . unwrapUnevaluated) pure


mkRegistry :: IsLinker -> Registry
mkRegistry linker =
    Registry
        (withNativeLinker linkerResolveUnqualified)
        (withNativeLinker linkerResolveAlgo)
  where
    withNativeLinker :: (Functor f, NativeConverter b) => (String -> Java IsLinker (f (NativeType b))) -> Binding -> f b
    withNativeLinker f = (fmap fromNative . pureJavaWith linker . f . bndToString)
    bndToString = T.unpack . unBinding
    stringToBinding = Binding . T.pack


nativeCompile :: IsLinker -> Object -> IO (NativeType OutGraph)
nativeCompile linker thing = toNative . fst <$> basicCompile linker thing


nativeCompileWSplice :: IsLinker -> Object -> IO (NGraph (NLazy Object))
nativeCompileWSplice linker thing = do
    (graph, envExprs0) <- basicCompile linker thing
    envExprs <- evalExprs linker envExprs0
    return $ toNative $ spliceEnv graph (Seq.index envExprs)


nativeCompileAlgo :: IsLinker -> Object -> IO NAlgo
nativeCompileAlgo linker thing = do
    (alang, envExprs) <- runM $ toALang (mkRegistry linker) st
    toNative . Algo alang <$> evalExprs linker envExprs
  where
    st = fromNative thing
    runM ac = fmap (either (error . T.unpack) id) $ runOhuaT0IO opts ac (definedBindings st)


nativeCompileWithoutEnvEval :: IsLinker -> Object -> IO (NGraph Object)
nativeCompileWithoutEnvEval linker thing = do
    (graph, envExprs) <- basicCompile linker thing
    return $ toNative $ spliceEnv graph (Seq.index $ fmap (either unwrapUnevaluated superCast) envExprs)


-- nativeToAlang :: Object -> IO ()
-- nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: IsLinker -> Object -> IO (NGraph JInteger)

foreign export java "@static ohua.Compiler.compileAndSpliceEnv" nativeCompileWSplice :: IsLinker -> Object -> IO (NGraph (NLazy Object))

foreign export java "@static ohua.Compiler.compileAlgo" nativeCompileAlgo :: IsLinker -> Object -> IO NAlgo
foreign export java "@static ohua.Compiler.compileWithoutEnvEval" nativeCompileWithoutEnvEval :: IsLinker -> Object -> IO (NGraph Object)

-- foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()

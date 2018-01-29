{-# LANGUAGE MagicHash #-}
module Ohua.Compat.JVM.Compiler where


import           Control.DeepSeq
import           Data.Default
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Sequence             as Seq
import           Java
import           Java.ConversionUtils
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.Compat.JVM.ClojureST
import           Ohua.Compat.JVM.Marshal
import qualified Ohua.Compat.JVM.Refs      as JVMRefs
import           Ohua.Compat.JVM.ToALang
import           Ohua.Compile
import           Ohua.DFGraph
import           Ohua.DFGraph.Show
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util
import qualified Ohua.Util.Str             as Str
import Ohua.Unit


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


basicCompile :: Object -> IsLinker -> Object -> IO (OutGraph, Seq (Either (Unevaluated Object) (NLazy Object)))
basicCompile loggingLevel linker thing =
  runStderrLoggingT $ filterLogger (\_ l -> l >= fromNative loggingLevel) $ do
    (graph, envExprs) <- runM $ do
      (alang, envExprs) <- toALang (mkRegistry linker) st
      forceLog "alang converted" alang
      logDebugN $ showT alang
            -- liftIO $ writeFile "alang-dump" $ show alang
      graph <- pipeline noCustomPasses {passAfterDFLowering = cleanUnits} alang
      forceLog "graph created" graph
      pure (graph, envExprs)
    logDebugN $ asTable $ graph
    pure (graph, envExprs)
  where
    st = fromNative thing
    runM ac = fmap (either (error . Str.toString) id) $ runFromBindings opts ac (definedBindings st)


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
    bndToString = Str.toString . unBinding
    stringToBinding = Binding . Str.fromString


nativeCompile :: Object -> IsLinker -> Object -> IO (NativeType OutGraph)
nativeCompile loggingLevel linker thing = toNative . fst <$> basicCompile loggingLevel linker thing


nativeCompileWSplice :: Object -> IsLinker -> Object -> IO (NGraph (NLazy Object))
nativeCompileWSplice logLevel linker thing = do
    (graph, envExprs0) <- basicCompile logLevel linker thing
    envExprs <- evalExprs linker envExprs0
    return $ toNative $ spliceEnv (Seq.index envExprs) graph


nativeCompileAlgo :: Object -> IsLinker -> Object -> IO NAlgo
nativeCompileAlgo logLevel linker thing = do
    (alang, envExprs) <- runM $ toALang (mkRegistry linker) st
    toNative . Algo alang <$> evalExprs linker envExprs
  where
    st = fromNative thing
    runM ac = runStderrLoggingT $ filterLogger (\_ l -> l >= fromNative logLevel) $ fmap (either (error . Str.toString) id) $ runFromBindings opts ac (definedBindings st)


nativeCompileWithoutEnvEval :: Object -> IsLinker -> Object -> IO (NGraph Object)
nativeCompileWithoutEnvEval logLevel linker thing = do
    (graph, envExprs) <- basicCompile logLevel linker thing
    return $ toNative $ spliceEnv (Seq.index $ fmap (either unwrapUnevaluated superCast) envExprs) graph


-- nativeToAlang :: Object -> IO ()
-- nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: Object -> IsLinker -> Object -> IO (NGraph JInteger)

foreign export java "@static ohua.Compiler.compileAndSpliceEnv" nativeCompileWSplice :: Object -> IsLinker -> Object -> IO (NGraph (NLazy Object))

foreign export java "@static ohua.Compiler.compileAlgo" nativeCompileAlgo :: Object -> IsLinker -> Object -> IO NAlgo
foreign export java "@static ohua.Compiler.compileWithoutEnvEval" nativeCompileWithoutEnvEval :: Object -> IsLinker -> Object -> IO (NGraph Object)

-- foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()

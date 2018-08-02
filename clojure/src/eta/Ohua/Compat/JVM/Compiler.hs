{-# LANGUAGE MagicHash, ConstraintKinds #-}
module Ohua.Compat.JVM.Compiler where


import           Control.DeepSeq
import           Data.Default
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Sequence             as Seq
import           Java
import           Java.ConversionUtils
import           Lens.Micro
import Lens.Micro.Mtl
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
import Control.Monad.Reader
import           Ohua.Types
import           Ohua.Util
import qualified Ohua.Util.Str             as Str
import Ohua.Unit
import Clojure.Core (Meta(Meta))


data NCompiler = NCompiler @ohua.Compiler
  deriving Class

data IsLinker = IsLinker @ohua.Linker
  deriving Class

-- TODO Move this to a more appropriate module at some point
instance NFData Meta where
  rnf (Meta m) = rnf m

foreign import java unsafe "@interface resolve" linkerResolveUnqualified :: String -> Java IsLinker (Maybe JString)
foreign import java unsafe "@interface resolveAlgo" linkerResolveAlgo :: String -> Java IsLinker (Maybe NAlgo)
foreign import java unsafe "@interface eval" linkerEval :: Object -> Java IsLinker (NLazy Object)

opts :: Options
opts = def
    & callEnvExpr .~ (Just JVMRefs.__callClojureFn)

forceA :: (NFData a, Applicative m) => a -> m ()
forceA = (`deepseq` pure ())


type CompMEnv = (CustomPasses Object, IsLinker, Object)
type CompM = ReaderT CompMEnv (LoggingT IO)

type HasCEnv m = MonadReader CompMEnv m

readLinker :: HasCEnv m => m IsLinker
readLinker = view _2

runCompM ::
       CompM a -> CustomPasses Object -> Object -> IsLinker -> Object -> IO a
runCompM comp passes loggingLevel linker o =
    runStderrLoggingT $
    filterLogger (\_ l -> l >= fromNative loggingLevel) $
    flip runReaderT (passes, linker, o) comp

runDefCompM :: CompM a -> Object -> IsLinker -> Object -> IO a
runDefCompM comp = runCompM comp def { passAfterDFLowering = cleanUnits }

readTarget :: HasCEnv m => m Object
readTarget = view _3


basicCompile :: CompM (OutGraph, JVMEnvExprs)
basicCompile = do
    passes <- view _1
    reg <- mkRegistry
    thing <- readTarget
    let st = fromNative thing
        runM ac =
            lift $
            fmap (either (error . Str.toString) id) $
            runFromBindings opts ac (definedBindings st)
    (graph, envExprs) <-
        runM $ do
            forceA st
            (alang, envExprs) <- toALang reg st
            forceLog "alang converted" alang
            logDebugN $ showT alang
            -- liftIO $ writeFile "alang-dump" $ show alang
            graph <- pipeline passes alang
            forceLog "graph created" graph
            pure (graph, envExprs)
    logDebugN $ asTable $ graph
    pure (graph, envExprs)


evalExprs :: (MonadIO m, HasCEnv m)
          => JVMEnvExprs
          -> m (Seq (NLazy Object))
evalExprs exprs = do
    linker <- readLinker
    mapM
        (either (liftIO . javaWith linker . linkerEval . unwrapUnevaluated) pure)
        exprs


mkRegistry :: HasCEnv m => m Registry
mkRegistry = do
    linker <- readLinker
    let withNativeLinker ::
               (Functor f, NativeConverter b)
            => (String -> Java IsLinker (f (NativeType b)))
            -> Binding
            -> f b
        withNativeLinker f =
            (fmap fromNative . unsafePerformJavaWith linker . f . bndToString)
    pure $
        Registry
            (withNativeLinker linkerResolveUnqualified)
            (withNativeLinker linkerResolveAlgo)
  where
    bndToString = Str.toString . unwrap


nativeCompile :: Object -> IsLinker -> Object -> IO (NativeType OutGraph)
nativeCompile = runDefCompM gNativeCompile

gNativeCompile :: CompM (NativeType OutGraph)
gNativeCompile = toNative . fst <$> basicCompile


nativeCompileWSplice :: Object -> IsLinker -> Object -> IO (NGraph (NLazy Object))
nativeCompileWSplice = runDefCompM gNativeCompileWSplice

gNativeCompileWSplice :: CompM (NGraph (NLazy Object))
gNativeCompileWSplice = do
    (graph, envExprs0) <- basicCompile
    envExprs <- evalExprs envExprs0
    pure $ toNative $ spliceEnv (Seq.index envExprs) graph


nativeCompileAlgo :: Object -> IsLinker -> Object -> IO NAlgo
nativeCompileAlgo = runDefCompM gNativeCompileAlgo

gNativeCompileAlgo :: CompM NAlgo
gNativeCompileAlgo = do
    reg <- mkRegistry
    thing <- readTarget
    let st = fromNative thing
        runM ac =
            lift $
            fmap (either (error . Str.toString) id) $
            runFromBindings opts ac (definedBindings st)
    (alang, envExprs) <- runM $ toALang reg st
    toNative . Algo alang <$> evalExprs envExprs


nativeCompileWithoutEnvEval :: Object -> IsLinker -> Object -> IO (NGraph Object)
nativeCompileWithoutEnvEval =
    runDefCompM $ do
        (graph, envExprs) <- basicCompile
        return $
            toNative $
            spliceEnv
                (Seq.index $ fmap (either unwrapUnevaluated superCast) envExprs)
                graph


-- nativeToAlang :: Object -> IO ()
-- nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: Object -> IsLinker -> Object -> IO (NGraph JInteger)

foreign export java "@static ohua.Compiler.compileAndSpliceEnv" nativeCompileWSplice :: Object -> IsLinker -> Object -> IO (NGraph (NLazy Object))

foreign export java "@static ohua.Compiler.compileAlgo" nativeCompileAlgo :: Object -> IsLinker -> Object -> IO NAlgo
foreign export java "@static ohua.Compiler.compileWithoutEnvEval" nativeCompileWithoutEnvEval :: Object -> IsLinker -> Object -> IO (NGraph Object)

-- foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()

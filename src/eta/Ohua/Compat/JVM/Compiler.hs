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



data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class

data {-# CLASS "ohua.Linker" #-} IsLinker = IsLinker (Object# IsLinker) deriving Class

foreign import java unsafe "@interface resolve" linkerResolveUnqualified :: String -> Java IsLinker (Maybe (NativeType QualifiedBinding))

nativeCompile :: IsLinker -> Object -> IO (NativeType OutGraph)
nativeCompile linker thing = toNative . either (error . T.unpack) id <$> compile (fromNative thing)
  where
    compile st = runOhuaT0IO (pipeline . fst =<< toALang registry st) (definedBindings st)
    registry = 
        SfRegistry 
        (fmap fromNative . pureJavaWith linker . linkerResolveUnqualified . bndToString)
    bndToString = T.unpack . unBinding
    stringToBinding = Binding . T.pack


spliceEnv :: OutGraph -> Seq Object -> AbstractOutGraph Object
spliceEnv (OutGraph ops oldArcs) objs = OutGraph ops arcs
  where
    arcs = map f oldArcs
    f (Arc t source) = Arc t $ case source of 
        EnvSource (HostExpr i) -> EnvSource (objs `Seq.index` i)
        LocalSource t -> LocalSource t

nativeCompileWSplice :: IsLinker -> Object -> IO (NGraph Object)
nativeCompileWSplice linker thing = toNative . either (error . T.unpack) id <$> compile (fromNative thing)
  where
    compile st = flip runOhuaT0IO (definedBindings st) $ do 
        (alang, envExprs) <- toALang registry st
        graph <- pipeline alang
        return $ spliceEnv graph envExprs
    registry = 
        SfRegistry 
        (fmap fromNative . pureJavaWith linker . linkerResolveUnqualified . bndToString)
    bndToString = T.unpack . unBinding
    stringToBinding = Binding . T.pack


-- nativeToAlang :: Object -> IO ()
-- nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: IsLinker -> Object -> IO (NativeType OutGraph)

foreign export java "@static ohua.Compiler.compileAndSpliceEnv" nativeCompileWSplice :: IsLinker -> Object -> IO (NGraph Object)

-- foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()


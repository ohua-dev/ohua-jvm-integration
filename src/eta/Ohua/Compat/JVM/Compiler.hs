{-# LANGUAGE MagicHash #-}
module Ohua.Compat.JVM.Compiler where


import Java
import Ohua.Compile
import Ohua.Monad
import Ohua.Compat.JVM.ToALang
import Ohua.Compat.JVM.Marshal
import Data.Foldable
import Ohua.Types



data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class

data {-# CLASS "ohua.support.Linker" #-} IsLinker = IsLinker (Object# IsLinker) deriving Class

foreign import java "@wrapper resolveUnqualified" linkerResolveUnqualified :: String -> Java IsLinker (Maybe String)
foreign import java "@wrapper resolveQualified" linkerResolveQualified :: String -> Java IsLinker Bool

nativeCompile :: IsLinker -> Object -> NGraph
nativeCompile linker = toNative . either error id . compile . fromNative
  where
    compile st = runOhuaT0 (pipeline . fst =<< toALang registry st) (definedBindings st)
    registry = 
        SfRegistry 
        (pureJavaWith linker . linkerResolveQualified . unBinding)
        (fmap Binding . pureJavaWith linker . linkerResolveUnqualified . unBinding)


nativeToAlang :: Object -> IO ()
nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: IsLinker -> Object -> NGraph

foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()


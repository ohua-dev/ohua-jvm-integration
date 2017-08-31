{-# LANGUAGE MagicHash #-}
module Ohua.Compat.JVM.Compiler where


import Java
import Ohua.Compile
import Ohua.Monad
import Ohua.Compat.JVM.ToALang
import Ohua.Compat.JVM.Marshal
import Data.Foldable



data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class


nativeCompile :: Object -> NGraph
nativeCompile = toNative . either error id . compile . fromNative
  where
    compile st = runOhuaT0 (pipeline . fst =<< toALang st) (definedBindings st)


nativeToAlang :: Object -> IO ()
nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: Object -> NGraph

foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()


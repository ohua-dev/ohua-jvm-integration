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



data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class

data {-# CLASS "ohua.support.Linker" #-} IsLinker = IsLinker (Object# IsLinker) deriving Class

foreign import java unsafe "@interface resolveUnqualified" linkerResolveUnqualified :: String -> Java IsLinker (Maybe String)
foreign import java unsafe "@interface resolveQualified" linkerResolveQualified :: String -> Java IsLinker Bool

nativeCompile :: IsLinker -> Object -> IO NGraph
nativeCompile linker thing = toNative . either (error . T.unpack) id <$> compile (fromNative thing)
  where
    compile st = runOhuaT0IO (pipeline . fst =<< toALang registry st) (definedBindings st)
    registry = 
        SfRegistry 
        (pureJavaWith linker . linkerResolveQualified . bndToString)
        (fmap stringToBinding . pureJavaWith linker . linkerResolveUnqualified . bndToString)
    bndToString = T.unpack . unBinding
    stringToBinding = Binding . T.pack


-- nativeToAlang :: Object -> IO ()
-- nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: IsLinker -> Object -> IO NGraph

-- foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()


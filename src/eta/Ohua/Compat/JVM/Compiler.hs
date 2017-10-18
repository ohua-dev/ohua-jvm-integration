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



data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class

data {-# CLASS "ohua.Linker" #-} IsLinker = IsLinker (Object# IsLinker) deriving Class

foreign import java unsafe "@interface resolve" linkerResolveUnqualified :: String -> Java IsLinker (Maybe JString)
foreign import java unsafe "@interface resolveAlgo" linkerResolveAlgo :: String -> Java IsLinker (Maybe NAlgo)

forceA :: (NFData a, Applicative m) => a -> m ()
forceA = (`deepseq` pure ())

basicCompile :: IsLinker -> Object -> IO (OutGraph, Seq Object)
basicCompile linker thing = do
    putStrLn "Compiler is running"
    let st = fromNative thing
    forceAndReport "ST was valid" st
    compiled <- fmap (either (error . T.unpack) id) . (`runOhuaT0IO` definedBindings st) $ do
        (alang, envExprs) <- toALang (mkRegistry linker) st
        forceAndReport "alang converted" alang
        liftIO $ print alang
        -- liftIO $ writeFile "alang-dump" $ show alang
        graph <- pipeline alang
        forceAndReport "graph created" graph
        pure (graph, envExprs)
    forceAndReport "Compilation done" $ fst compiled
    pure compiled


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


nativeCompileWSplice :: IsLinker -> Object -> IO (NGraph Object)
nativeCompileWSplice linker thing = do
    (graph, envExprs) <- basicCompile linker thing
    return $ toNative $ spliceEnv graph (Seq.index envExprs)


nativeCompileAlgo :: IsLinker -> Object -> IO NAlgo
nativeCompileAlgo linker thing =
    fmap (either (error . T.unpack) (toNative . uncurry Algo)) 
        . (`runOhuaT0IO` definedBindings st) 
        $ toALang (mkRegistry linker) st 
  where st = fromNative thing


-- nativeToAlang :: Object -> IO ()
-- nativeToAlang = either error (\(alang, objects) -> print alang >> print (toList objects)) . (\st -> runOhuaT0 (toALang st) (definedBindings st)) . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: IsLinker -> Object -> IO (NGraph JInteger)

foreign export java "@static ohua.Compiler.compileAndSpliceEnv" nativeCompileWSplice :: IsLinker -> Object -> IO (NGraph Object)

foreign export java "@static ohua.Compiler.compileAlgo" nativeCompileAlgo :: IsLinker -> Object -> IO NAlgo

-- foreign export java "@static ohua.Compiler.testToALang" nativeToAlang :: Object -> IO ()

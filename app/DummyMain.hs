{-# LANGUAGE BangPatterns, FlexibleContexts, StandaloneDeriving #-}
import Ohua.Compat.JVM.Compiler
import Java
import qualified Clojure
import Ohua.Monad
import Data.Functor.Identity
import Ohua.Compile
import Ohua.Compat.JVM.ToALang
import Ohua.Compat.JVM.Marshal
import Ohua.DFGraph
import Debug.Trace
import System.IO
import Control.Monad.Except


deriving instance Show OutGraph
deriving instance Show Operator
deriving instance Show Arc
deriving instance Show Target
deriving instance Show Source

main = do 
    hPutStrLn stderr "Starting"
    hPutStrLn stderr "I will now read"
    !r <- Clojure.read "(a b c)"
    hPutStrLn stderr $ fromJava $ toString r
    hPutStrLn stderr "I did read"
    let !converted = fromNative r
    hPutStrLn stderr "I converted to haskell"
    let !bnds = definedBindings converted

    !c <- runExceptT $ flip runOhuaT0 bnds $ do
        liftIO $ hPutStrLn stderr $ "Running actual compiler"
        !alang <- toALang converted
        liftIO $ hPutStrLn stderr $ "Converted to alang"
        !p <- pipeline $ fst alang
        liftIO $ hPutStrLn stderr $ "Ran pipeline"
        return p
    hPutStrLn stderr "I'm done compiling"
    let !native = toNative $ either error id c
    hPutStrLn stderr "I converted to native"
    putStrLn $ fromJava $ toString native
    hPutStrLn stderr "done"

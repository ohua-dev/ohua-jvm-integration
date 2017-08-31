{-# LANGUAGE BangPatterns, FlexibleContexts, StandaloneDeriving, OverloadedStrings #-}
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
import Ohua.Compat.JVM.ClojureST
import Control.DeepSeq


deriving instance Show OutGraph
deriving instance Show Operator
deriving instance Show Arc
deriving instance Show Target
deriving instance Show Source
deriving instance Show ST
deriving instance Show Vector
deriving instance Show Symbol


foreign import java "getClass" getClass_ :: Java Object (JClass Object)


main = do 
    !r <- Clojure.read "(let [a (print b)] a)"
    let converted = fromNative r
    hPutStrLn stderr $ fromJava $ toString r
    converted `deepseq` return ()    
    hPutStrLn stderr $ show converted

    -- let Form l = converted
    -- hPutStrLn stderr $ show $ length l
    -- hPutStrLn stderr $ show $ let Sym s = l !! 0 in s == Symbol Nothing "a"
    -- hPutStrLn stderr $ show $ let Sym s = l !! 1 in s == Symbol Nothing "b"
    -- -- hPutStrLn stderr $ show $ let Sym s = l !! 2 in s == Symbol Nothing "c"
    -- hPutStrLn stderr $ let Form l = converted in show $ l !! 0
    -- hPutStrLn stderr $ let Form l = converted in show $ l !! 1
    -- hPutStrLn stderr $ let Form l = converted in show $ l !! 2
    -- hPutStrLn stderr $ "\n\n" ++ show converted ++ "\n\n\n"
    let !bnds = definedBindings converted

    !c <- runExceptT $ flip runOhuaT0 bnds $ do
        !alang <- toALang converted
        !p <- pipeline $ fst alang
        return p
    let !native = toNative $ either error id c
    putStrLn $ fromJava $ toString native

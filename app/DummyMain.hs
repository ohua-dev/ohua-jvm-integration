{-# LANGUAGE BangPatterns, FlexibleContexts, StandaloneDeriving, OverloadedStrings, OverloadedLists #-}
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
    let reg = SfRegistry ["some.module/print"] [("print", "some.module/print")]
    hPutStrLn stderr $ fromJava $ toString r
    converted `deepseq` return ()    
    hPutStrLn stderr $ show converted

    let !bnds = definedBindings converted
    c <- runExceptT $ flip runOhuaT0 bnds $ toALang converted
    print c

    -- let !bnds = definedBindings converted

    -- !c <- runExceptT $ flip runOhuaT0 bnds $ do
    --     !alang <- toALang converted
    --     !p <- pipeline $ fst alang
    --     return p
    -- let !native = toNative $ either error id c
    -- putStrLn $ fromJava $ toString native

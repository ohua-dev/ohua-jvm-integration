{-# LANGUAGE BangPatterns #-}
import Ohua.Compat.JVM.Compiler
import Java
import qualified Clojure


main = putStrLn . fromJava . toString . nativeCompile =<< Clojure.read "(a b c)"
    

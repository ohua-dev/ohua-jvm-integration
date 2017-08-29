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


deriving instance Show OutGraph
deriving instance Show Operator
deriving instance Show Arc
deriving instance Show Target
deriving instance Show Source


-- main = putStrLn . fromJava . toString . nativeCompile =<< Clojure.read "(a b c)"

-- main = print . either error id . compile . fromNative =<< Clojure.read "(a b c)"
--   where
--     compile st = runOhuaT0 (pipeline . fst =<< toALang st) (definedBindings st)
    

main = print =<< runOhuaT0 generateBinding mempty

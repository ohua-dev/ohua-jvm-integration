{-# LANGUAGE BangPatterns #-}
import Ohua.Compat.JVM.Compiler
import Java
import Unsafe.Coerce

foreign import java "Object.toString" toString0 :: Java Object String


main = do
    let !x = 0 :: Int 
    let !e = []
    let !l = x:e
    putStrLn =<< javaWith (unsafeCoerce l) toString0

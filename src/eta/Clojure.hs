{-# LANGUAGE MagicHash #-}
module Clojure where

import Java
import System.IO.Unsafe

data {-# CLASS "clojure.lang.IFn" #-} IFn = IFn (Object# IFn) deriving Class

foreign import java "clojure.lang.IFn.invoke" invoke0 :: Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke1 :: Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke2 :: Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke3 :: Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke4 :: Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke5 :: Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke6 :: Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke7 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke8 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke9 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke10 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke11 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke12 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke13 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke14 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke15 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke16 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke17 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke18 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke19 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke20 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "clojure.lang.IFn.invoke" invoke :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> JObjectArray -> Java IFn Object

foreign import java "@static clojure.java.api.Clojure/var" varNS :: String -> String -> IO IFn
foreign import java "@static clojure.java.api.Clojure/var" var :: String -> IO IFn
foreign import java "@static clojure.java.api.Clojure/read" read :: String -> IO Object

coreVar :: String -> IFn
coreVar = unsafePerformIO . varNS "clojure.core"

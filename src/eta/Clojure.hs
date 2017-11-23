{-# LANGUAGE MagicHash, BangPatterns #-}
module Clojure where

import Java
import System.IO.Unsafe
import System.IO
import Data.Maybe

data {-# CLASS "clojure.lang.IFn" #-} IFn = IFn (Object# IFn) deriving Class

foreign import java "@interface invoke" invoke0 :: Java IFn Object
foreign import java "@interface invoke" invoke1 :: Object -> Java IFn Object
foreign import java "@interface invoke" invoke2 :: Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke3 :: Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke4 :: Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke5 :: Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke6 :: Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke7 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke8 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke9 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke10 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke11 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke12 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke13 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke14 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke15 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke16 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke17 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke18 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke19 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke20 :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Java IFn Object
foreign import java "@interface invoke" invoke :: Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> Object -> JObjectArray -> Java IFn Object

foreign import java unsafe "@static clojure.java.api.Clojure.var" mVarNS :: Object -> Object -> IO (Maybe IFn)
foreign import java unsafe "@static clojure.java.api.Clojure.var" mVar :: Object -> IO (Maybe IFn)
foreign import java unsafe "@static clojure.java.api.Clojure.read" mRead :: String -> IO (Maybe Object)

varNS :: String -> String -> IO IFn
varNS name namespace = fromMaybe (error $ "Var not found: " ++ name ++ "/" ++ namespace) <$> mVarNS (strToObj name) (strToObj namespace)
  where
    strToObj = (superCast :: JString -> Object) . toJava

var :: String -> IO IFn
var name = fromMaybe (error $ "Var not found: " ++ name) <$> mVar ((superCast :: JString -> Object) $ toJava name)

read :: String -> IO Object
read s = fromMaybe (error $ "Could not read " ++ s) <$> mRead s

coreVar :: String -> IFn
coreVar v = unsafeDupablePerformIO $ varNS "clojure.core" v

keyword :: String -> Object
keyword = pureJavaWith (coreVar "keyword") . invoke1 . (superCast :: JString -> Object) . toJava

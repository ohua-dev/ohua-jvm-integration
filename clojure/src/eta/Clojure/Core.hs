{-# LANGUAGE BangPatterns #-}
module Clojure.Core where


import           Clojure
import qualified Data.Text            as T
import           Java
import           Java.ConversionUtils
import qualified Ohua.Util.Str        as Str
import           System.IO.Unsafe


newtype Meta = Meta (Maybe Object) deriving Show


isSeq :: Object -> Bool
isSeq o = asBool $ unsafePerformJavaWith (Clojure.coreVar "seq?") $ Clojure.invoke1 o

isSymbol :: Object -> Bool
isSymbol = asBool . unsafePerformJavaWith (Clojure.coreVar "symbol?") . Clojure.invoke1

isVector :: Object -> Bool
isVector = asBool . unsafePerformJavaWith (Clojure.coreVar "vector?") . Clojure.invoke1

name :: Object -> Str.Str
name = asString . unsafePerformJavaWith (Clojure.coreVar "name") . Clojure.invoke1

namespace :: Object -> Maybe Str.Str
namespace = fmap Str.fromString . (maybeFromJava :: JString -> Maybe String) . unsafeCast . unsafePerformJavaWith (Clojure.coreVar "namespace") . Clojure.invoke1

asSeq :: Object -> Object
asSeq = unsafePerformJavaWith (Clojure.coreVar "seq") . Clojure.invoke1

vector :: Object -> Object
vector = unsafePerformJavaWith (Clojure.coreVar "vec") . Clojure.invoke1

eq :: Object -> Object -> Bool
eq o1 o2 = asBool $ unsafePerformJavaWith (Clojure.coreVar "=") $ Clojure.invoke2 o1 o2

isKw :: Object -> Bool
isKw = asBool . unsafePerformJavaWith (Clojure.coreVar "keyword?") . Clojure.invoke1

meta :: Object -> Meta
meta = Meta . maybeFromJava . unsafePerformJavaWith (Clojure.coreVar "meta") . Clojure.invoke1

get :: Object -> Object -> Maybe Object
get map key = maybeFromJava $ unsafePerformJavaWith (Clojure.coreVar "get") $ Clojure.invoke2 map key

supportsMeta :: Object -> Bool
supportsMeta o = asBool $ unsafePerformJavaWith supportsMetaCljFun $ Clojure.invoke1 o
  where
    supportsMetaCljFun = unsafeCast $ unsafeDupablePerformIO $ Clojure.read "(fn [o] (instance? IObj o))"

withMeta :: Object -> Meta -> Object
withMeta o (Meta meta) | supportsMeta o = unsafePerformJavaWith (Clojure.coreVar "with-meta") $ Clojure.invoke2 o (maybeToJava meta)
withMeta o _ = o

type_ :: Object -> Object
type_ = unsafePerformJavaWith (Clojure.coreVar "type") . Clojure.invoke1


keyword :: String -> Object
keyword = unsafePerformJavaWith (coreVar "keyword") . invoke1 . (superCast :: JString -> Object) . toJava

module Clojure.Core where


import Java
import Clojure
import Java.ConversionUtils
import qualified Data.Text as T
import qualified Ohua.Util.Str as Str


isSeq :: Object -> Bool
isSeq o = asBool $ pureJavaWith (Clojure.coreVar "seq?") $ Clojure.invoke1 o

isSymbol :: Object -> Bool
isSymbol = asBool . pureJavaWith (Clojure.coreVar "symbol?") . Clojure.invoke1

isVector :: Object -> Bool
isVector = asBool . pureJavaWith (Clojure.coreVar "vector?") . Clojure.invoke1

name :: Object -> Str.Str
name = asString . pureJavaWith (Clojure.coreVar "name") . Clojure.invoke1

namespace :: Object -> Maybe Str.Str
namespace = fmap Str.fromString . (maybeFromJava :: JString -> Maybe String) . unsafeCast . pureJavaWith (Clojure.coreVar "namespace") . Clojure.invoke1

asSeq :: Object -> Object
asSeq = pureJavaWith (Clojure.coreVar "seq") . Clojure.invoke1

vector :: Object -> Object
vector = pureJavaWith (Clojure.coreVar "vec") . Clojure.invoke1

eq :: Object -> Object -> Bool
eq o1 o2 = asBool $ pureJavaWith (Clojure.coreVar "=") $ Clojure.invoke2 o1 o2

isKw :: Object -> Bool
isKw = asBool . pureJavaWith (Clojure.coreVar "keyword?") . Clojure.invoke1

meta :: Object -> Object
meta = pureJavaWith (Clojure.coreVar "meta") . Clojure.invoke1

get :: Object -> Object -> Maybe Object 
get map key = maybeFromJava $ pureJavaWith (Clojure.coreVar "get") $ Clojure.invoke2 map key

withMeta :: Object -> Object -> Object
withMeta o meta = pureJavaWith (Clojure.coreVar "with-meta") $ Clojure.invoke2 o meta

type_ :: Object -> Object
type_ = pureJavaWith (Clojure.coreVar "type") . Clojure.invoke1


keyword :: String -> Object
keyword = pureJavaWith (coreVar "keyword") . invoke1 . (superCast :: JString -> Object) . toJava

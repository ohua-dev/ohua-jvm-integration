module MarshalTests where


import Ohua.ALang.Lang
import Ohua.Compat.JVM.Marshal
import Ohua.Compat.JVM.ToAlang
import Clojure
import Java


cljRead :: String -> Object
cljRead str = pureJavaWith (Clojure.coreVar "read-string") $ Clojure.invoke1 (superCast (toJava str :: JString))



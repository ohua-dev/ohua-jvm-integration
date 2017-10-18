module Registry where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Ohua.Compat.JVM.ToALang
import Ohua.Types
import Ohua.Compat.JVM.ClojureST


simpleRegistry :: HM.HashMap Binding QualifiedBinding -> HM.HashMap Binding Algo -> Registry
simpleRegistry map1 map2 = Registry (`HM.lookup` map1) (`HM.lookup` map2)

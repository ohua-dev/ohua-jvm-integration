module Registry where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Ohua.Compat.JVM.ToALang
import Ohua.Types


simpleRegistry :: HM.HashMap Binding FnName -> SfRegistry
simpleRegistry map = SfRegistry (`HM.lookup` map)

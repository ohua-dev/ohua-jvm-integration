module Registry where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Ohua.Compat.JVM.ToALang
import Ohua.Types


simpleRegistry :: HS.HashSet Binding -> HM.HashMap Binding Binding -> SfRegistry
simpleRegistry set map = SfRegistry (`HS.member` set) (`HM.lookup` map)

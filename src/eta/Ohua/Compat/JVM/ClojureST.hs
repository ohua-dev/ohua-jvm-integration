module Ohua.Compat.JVM.ClojureST where


import Java
import Data.Hashable


data ST 
    = Literal Object
    | Form [ST]
    | Sym Symbol
    | Vec Vector


data Symbol = Symbol 
    { namespace :: Maybe String 
    , name :: String
    } deriving Eq

newtype Vector = Vector { vectorToList :: [ST] }

instance Hashable Symbol where
    hashWithSalt s (Symbol ns n) = hashWithSalt s (ns, n)


class ToEnvExpr a where
    toEnvExpr :: a -> Object

instance ToEnvExpr Object where
    toEnvExpr = id

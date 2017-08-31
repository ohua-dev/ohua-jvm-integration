module Ohua.Compat.JVM.ClojureST where


import Java
import Data.Hashable
import qualified Data.Text as T
import Control.DeepSeq


data ST 
    = Literal Object
    | Form [ST]
    | Sym Symbol
    | Vec Vector

instance NFData ST where
    rnf (Literal o) = ()
    rnf (Form l) = l `deepseq` ()
    rnf (Sym s) = s `deepseq` ()
    rnf (Vec v) = v `deepseq` ()

data Symbol = Symbol 
    { namespace :: Maybe T.Text 
    , name :: T.Text
    } deriving Eq

instance NFData Symbol where
    rnf (Symbol ns n) = ns `deepseq` n `deepseq` ()

newtype Vector = Vector { vectorToList :: [ST] }

instance NFData Vector where
    rnf (Vector v) = v `deepseq` ()

instance Hashable Symbol where
    hashWithSalt s (Symbol ns n) = hashWithSalt s (ns, n)


class ToEnvExpr a where
    toEnvExpr :: a -> Object

instance ToEnvExpr Object where
    toEnvExpr = id

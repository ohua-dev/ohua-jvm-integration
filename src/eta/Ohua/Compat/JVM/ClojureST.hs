module Ohua.Compat.JVM.ClojureST where


import           Control.DeepSeq
import           Data.Hashable
import qualified Data.Text       as T
import           Java
import Ohua.ALang.Lang (Expression)
import qualified Data.Sequence as S


data ST
    = Literal Object
    | Form [ST]
    | Sym Symbol
    | Vec Vector

instance NFData ST where
    rnf (Literal o) = ()
    rnf (Form l)    = l `deepseq` ()
    rnf (Sym s)     = s `deepseq` ()
    rnf (Vec v)     = v `deepseq` ()

instance Eq ST where
    Form f1 == Form f2 = f1 == f2
    Sym s1 == Sym s2 = s1 == s2
    Vec v1 == Vec v2 = v1 == v2
    Literal l1 == Literal l2 = True -- FIXME
    _ == _ = False

data Symbol = Symbol
    { namespace :: Maybe T.Text
    , name      :: T.Text
    } deriving (Eq, Show)

instance NFData Symbol where
    rnf (Symbol ns n) = ns `deepseq` n `deepseq` ()

newtype Vector = Vector { vectorToList :: [ST] } deriving Eq

instance NFData Vector where
    rnf (Vector v) = v `deepseq` ()

instance Hashable Symbol where
    hashWithSalt s (Symbol ns n) = hashWithSalt s (ns, n)


class ToEnvExpr a where
    toEnvExpr :: a -> Object

instance ToEnvExpr Object where
    toEnvExpr = id


data Algo = Algo !Expression !(S.Seq Object)

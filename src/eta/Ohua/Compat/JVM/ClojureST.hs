{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ohua.Compat.JVM.ClojureST where


import           Control.DeepSeq
import           Data.Hashable
import           Data.List        (intercalate)
import           Data.Monoid
import qualified Data.Sequence    as S
import qualified Data.Text        as T
import           Java
import           Lens.Micro
import           Ohua.ALang.Lang  (Expression)
import           Ohua.LensClasses
import           Ohua.Types
import           Ohua.Util


data GenericST a
    = Literal Object
    | Form [a]
    | Sym Symbol
    | Vec (Vector a)

instance Show a => Show (GenericST a) where
    show (Literal _)  = "Object"
    show (Form exprs) = "(" <> intercalate " " (map show exprs) <> ")"
    show (Sym s)      = show s
    show (Vec v)      = show v

instance NFData a => NFData (GenericST a) where
    rnf (Literal o) = ()
    rnf (Form l)    = l `deepseq` ()
    rnf (Sym s)     = s `deepseq` ()
    rnf (Vec v)     = v `deepseq` ()

instance Eq a => Eq (GenericST a) where
    Form f1 == Form f2 = f1 == f2
    Sym s1 == Sym s2 = s1 == s2
    Vec v1 == Vec v2 = v1 == v2
    Literal l1 == Literal l2 = equals l1 l2
    _ == _ = False

instance NFData Object where
    rnf _ = ()

newtype ST = ST { stToGeneric :: GenericST ST } deriving (Show, Eq, NFData)

newtype AnnotatedST a = AnnotatedST { unwrapAnnotatedST :: Annotated a (GenericST (AnnotatedST a)) } deriving (Show, Eq, NFData)

intoAnn ::
    Lens
        (AnnotatedST a)
        (AnnotatedST b)
        (Annotated a (GenericST (AnnotatedST a)))
        (Annotated b (GenericST (AnnotatedST b)))
intoAnn f s = AnnotatedST <$> f (unwrapAnnotatedST s)

instance HasAnnotation (AnnotatedST a) a where
    annotation = intoAnn . annotation

instance HasValue (AnnotatedST a) (GenericST (AnnotatedST a)) where
    value = intoAnn . value

data Symbol = Symbol
    { namespace :: Maybe T.Text
    , name      :: T.Text
    } deriving (Eq, Show)

instance NFData Symbol where
    rnf (Symbol ns n) = ns `deepseq` n `deepseq` ()

instance ShowT Symbol where
    showT (Symbol ns name) = maybe "" (<> "/") ns <> name

newtype Vector st = Vector { vectorToList :: [st] } deriving Eq

instance Show st => Show (Vector st) where
    show (Vector exprs) = "[" <> intercalate " " (map show exprs) <> "]"

instance NFData st => NFData (Vector st) where
    rnf (Vector v) = v `deepseq` ()

instance Hashable Symbol where
    hashWithSalt s (Symbol ns n) = hashWithSalt s (ns, n)


class ToEnvExpr a where
    toEnvExpr :: a -> Object

instance ToEnvExpr Object where
    toEnvExpr = id

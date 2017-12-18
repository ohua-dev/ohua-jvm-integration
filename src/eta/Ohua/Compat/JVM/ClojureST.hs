{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Ohua.Compat.JVM.ClojureST where


import           Control.DeepSeq
import           Data.Hashable
import           Data.List        (intercalate)
import           Data.Monoid
import qualified Data.Sequence    as S
import           Java
import           Lens.Micro
import           Ohua.ALang.Lang  (Expression)
import           Ohua.LensClasses
import           Ohua.Types
import           Ohua.Util
import qualified Ohua.Util.Str as Str
import Data.Functor.Classes (Eq1(liftEq), eq1, Show1, showsPrec1)
import Data.Functor.Identity
import Data.Foldable (for_)


data GenericST f
    = Lit Object
    | Form [f (GenericST f)]
    | Sym Symbol
    | Vec [f (GenericST f)]

instance Show1 a => Show (GenericST a) where
    showsPrec _ (Lit _)  = showString "Object"
    showsPrec p (Form exprs) = 
        showString "(" .
        (case exprs of
            [] -> id
            x:xs -> foldl (\f e -> f . showsPrec1 0 e . showString " " ) (showsPrec1 0 x) xs) .
        showString ")"
    showsPrec p (Sym s)      = showsPrec p s
    showsPrec _ (Vec exprs)      =
        showString "[" .
        (case exprs of
            [] -> id
            x:xs -> foldl (\f e -> f . showsPrec1 0 e . showString " " ) (showsPrec1 0 x) xs) .
        showString "]"

instance NFData (f (GenericST f)) => NFData (GenericST f) where
    rnf (Lit o) = ()
    rnf (Form l)    = l `deepseq` ()
    rnf (Sym s)     = s `deepseq` ()
    rnf (Vec v)     = v `deepseq` ()

instance Eq1 a => Eq (GenericST a) where
    Form f1 == Form f2 = liftEq eq1 f1 f2
    Sym s1 == Sym s2 = s1 == s2
    Vec v1 == Vec v2 = liftEq eq1 v1 v2
    Lit l1 == Lit l2 = equals l1 l2
    _ == _ = False

instance NFData Object where
    rnf _ = ()

newtype ST = ST { stToGeneric :: GenericST Identity } deriving (Show, Eq, NFData)

type AnnotatedST a = Annotated a (GenericST (Annotated a))

data Symbol = Symbol
    { namespace :: Maybe Str.Str
    , name      :: Str.Str
    } deriving (Eq, Show)

instance NFData Symbol where
    rnf (Symbol ns n) = ns `deepseq` n `deepseq` ()

-- instance ShowT Symbol where
--     showT (Symbol ns name) = T.pack $ Str.toString $ maybe "" (<> "/") ns <> name

instance Hashable Symbol where
    hashWithSalt s (Symbol ns n) = hashWithSalt s (ns, n)


class ToEnvExpr a where
    toEnvExpr :: a -> Object

instance ToEnvExpr Object where
    toEnvExpr = id

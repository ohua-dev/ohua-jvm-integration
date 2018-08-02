{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Ohua.Compat.JVM.ClojureST where


import           Control.DeepSeq
import           Data.Foldable         (foldr', for_)
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Data.Functor.Foldable
import           Data.Hashable
import           Data.List             (intercalate)
import           Data.Monoid
import qualified Data.Sequence         as S
import           Java
import           Lens.Micro
import           Ohua.ALang.Lang       (Expression)
import           Ohua.LensClasses
import           Ohua.Types
import           Ohua.Util
import qualified Ohua.Util.Str         as Str


data ExprF a
    = LitF  Object
    | FormF [a]
    | SymF  Symbol
    | VecF  [a]
    deriving (Eq, Functor, Traversable, Foldable)

instance NFData a => NFData (ExprF a) where
  rnf = foldr' deepseq ()

newtype ST = ST (ExprF ST)

type instance Base ST = ExprF

instance Recursive ST where
  project (ST b) = b

instance Corecursive ST where
  embed = ST

pattern Lit o = ST (LitF o)
pattern Form l = ST (FormF l)
pattern Sym s = ST (SymF s)
pattern Vec v = ST (VecF v)

pattern AnnLitF ann o = Compose (Annotated ann (LitF o))
pattern AnnFormF ann f = Compose (Annotated ann (FormF f))
pattern AnnSymF ann s = Compose (Annotated ann (SymF s))
pattern AnnVecF ann v = Compose (Annotated ann (VecF v))

pattern AnnLit ann o  = AnnST (AnnLitF ann o)
pattern AnnForm ann f = AnnST (AnnFormF ann f)
pattern AnnSym ann s  = AnnST (AnnSymF ann s)
pattern AnnVec ann v  = AnnST (AnnVecF ann v)

instance Show ST where
  showsPrec _ (Lit _) = showString "Object"
  showsPrec p (Form exprs) =
    showString "(" .
    (case exprs of
       [] -> id
       x:xs -> foldl (\f e -> f . showsPrec 0 e . showString " " ) (showsPrec 0 x) xs) .
    showString ")"
  showsPrec p (Sym s) = showsPrec p s
  showsPrec _ (Vec exprs) =
    showString "[" .
    (case exprs of
       [] -> id
       x:xs -> foldl (\f e -> f . showsPrec 0 e . showString " " ) (showsPrec 0 x) xs) .
    showString "]"

instance Show a => Show (ExprF a) where
  showsPrec _ (LitF _) = showString "Object"
  showsPrec p (FormF exprs) =
    showString "(" .
    (case exprs of
       [] -> id
       x:xs -> foldl (\f e -> f . showsPrec 0 e . showString " " ) (showsPrec 0 x) xs) .
    showString ")"
  showsPrec p (SymF s) = showsPrec p s
  showsPrec _ (VecF exprs) =
    showString "[" .
    (case exprs of
       [] -> id
       x:xs -> foldl (\f e -> f . showsPrec 0 e . showString " " ) (showsPrec 0 x) xs) .
    showString "]"


instance NFData ST where rnf (ST s) = rnf s

instance NFData Object where
    rnf _ = ()

newtype AnnST ann = AnnST (Compose (Annotated ann) ExprF (AnnST ann))

instance NFData ann => NFData (AnnST ann) where
  rnf (AnnST (Compose inner)) = rnf inner

type instance Base (AnnST ann) = Compose (Annotated ann) ExprF

instance Recursive (AnnST ann) where
  project (AnnST b) = b

instance Corecursive (AnnST ann) where
  embed = AnnST

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

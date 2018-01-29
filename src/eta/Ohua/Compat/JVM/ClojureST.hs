{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Ohua.Compat.JVM.ClojureST where


import           Control.DeepSeq
import           Data.Foldable         (for_)
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


data Expr a
    = Lit  Object
    | Form [a]
    | Sym  Symbol
    | Vec  [a]
    deriving (Eq, Functor)

pattern AnnLit ann o  = Compose (Annotated ann (Lit o))
pattern AnnForm ann f = Compose (Annotated ann (Form f))
pattern AnnSym ann s  = Compose (Annotated ann (Sym s))
pattern AnnVec ann v  = Compose (Annotated ann (Vec v))

instance Show a => Show (Expr a) where
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

instance Show1 Expr where
  liftShowsPrec showInner _ p = \case
    Lit _ -> showString "Object"
    Form exprs ->
      showString "(" .
      (case exprs of
         [] -> id
         x:xs -> foldl (\f e -> f . showInner 0 e . showString " " ) (showInner 0 x) xs) .
      showString ")"
    Sym s -> showsPrec p s
    Vec exprs ->
      showString "[" .
      (case exprs of
         [] -> id
         x:xs -> foldl (\f e -> f . showInner 0 e . showString " " ) (showInner 0 x) xs) .
      showString "]"


instance NFData a => NFData (Expr a) where
    rnf (Lit o)  = ()
    rnf (Form l) = l `deepseq` ()
    rnf (Sym s)  = s `deepseq` ()
    rnf (Vec v)  = v `deepseq` ()

instance NFData Object where
    rnf _ = ()

type ST = Fix Expr

lit :: Object -> ST
lit = Fix . Lit

form :: [ST] -> ST
form = Fix . Form

sym :: Symbol -> ST
sym = Fix . Sym

vec :: [ST] -> ST
vec = Fix . Vec

type AnnST ann = Fix (Compose (Annotated ann) Expr)

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

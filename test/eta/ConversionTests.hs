{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving #-}
module ConversionTests where

import Test.QuickCheck
import Ohua.Compat.JVM.Marshal
import Data.Proxy
import Ohua.Compat.JVM.ClojureST
import Ohua.ALang.Lang
import Ohua.DFGraph
import Ohua.Types
import Control.Monad
import qualified Data.Text as T
import System.Exit


deriving instance Show Vector
deriving instance Show OutGraph
deriving instance Show Arc
deriving instance Show Target
deriving instance Show Source
deriving instance Show Symbol
deriving instance Show Operator
deriving instance Show ST


instance Arbitrary ST where
    arbitrary = oneof
        [ Form <$> arbitrary
        , Sym <$> arbitrary
        , Vec <$> arbitrary
        ]
    
    shrink (Form f) = f ++ concatMap shrink f
    shrink (Vec (Vector v)) = v ++ concatMap shrink v
    shrink _ = []

instance Arbitrary T.Text where arbitrary = T.pack <$> arbitrary
instance Arbitrary Vector where arbitrary = Vector <$> arbitrary
instance Arbitrary Symbol where arbitrary = Symbol <$> arbitrary <*> arbitrary
instance Arbitrary OutGraph where arbitrary = OutGraph <$> arbitrary <*> arbitrary
instance Arbitrary Operator where arbitrary = Operator <$> arbitrary <*> arbitrary
instance Arbitrary Target where arbitrary = Target <$> arbitrary <*> arbitrary
instance Arbitrary Source where arbitrary = oneof [LocalSource <$> arbitrary, EnvSource <$> arbitrary]
instance Arbitrary Arc where arbitrary = Arc <$> arbitrary <*> arbitrary
instance Arbitrary Binding where arbitrary = Binding <$> arbitrary
instance Arbitrary FnName where arbitrary = FnName <$> arbitrary
instance Arbitrary FnId where arbitrary = FnId <$> arbitrary
instance Arbitrary HostExpr where arbitrary = HostExpr <$> arbitrary
instance Arbitrary ResolvedSymbol where
    arbitrary = oneof
        [ Local <$> arbitrary
        , Sf <$> arbitrary <*> arbitrary
        , Env <$> arbitrary
        ]
instance Arbitrary Assignment where
    arbitrary = oneof [Direct <$> arbitrary, Destructure <$> arbitrary]
instance Arbitrary a => Arbitrary (Expr a) where
    arbitrary = sized expr
      where
        expr 0 = Var <$> arbitrary
        expr n = oneof
            [ liftM3 Let arbitrary nestExpr nestExpr
            , liftM2 Apply nestExpr nestExpr
            , liftM2 Lambda arbitrary nestExpr
            , Var <$> arbitrary
            ]
          where
            nestExpr = expr $ n `div` 2
    shrink (Let _ _ b) = b: shrink b
    shrink (Apply _ a) = a:shrink a
    shrink (Lambda _ a) = a:shrink a
    shrink _ = []


type Check a = a -> Bool

mkConversionSpec :: (Arbitrary a, NativeConverter a, Eq a, Show a) => Check a
mkConversionSpec a = fromNative (toNative a) == a



testAll :: IO ()
testAll = do
    quickCheck (mkConversionSpec :: Check ST)
    quickCheck (mkConversionSpec :: Check Expression)
    quickCheck (mkConversionSpec :: Check OutGraph)
    exitWith (ExitFailure 1)

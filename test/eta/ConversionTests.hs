{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances
           , FlexibleInstances, FlexibleContexts, OverloadedStrings, OverloadedLists #-}
module ConversionTests where

import Test.QuickCheck
import Ohua.Compat.JVM.Marshal
import Data.Proxy
import Ohua.Compat.JVM.ClojureST
import Ohua.ALang.Lang hiding (Symbol)
import Ohua.DFGraph
import Ohua.Types
import Control.Monad
import qualified Data.Text as T
import System.Exit
import Control.Applicative
import qualified Data.Vector as V


deriving instance Show Vector
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
instance Arbitrary (Source HostExpr) where arbitrary = oneof [LocalSource <$> arbitrary, EnvSource <$> arbitrary]
instance Arbitrary (Arc HostExpr) where arbitrary = Arc <$> arbitrary <*> arbitrary
instance Arbitrary Binding where arbitrary = Binding <$> arbitrary
instance Arbitrary FnId where arbitrary = FnId <$> arbitrary
instance Arbitrary QualifiedBinding where arbitrary = liftA2 QualifiedBinding arbitrary arbitrary
instance Arbitrary NSRef where arbitrary = NSRef <$> arbitrary
instance Arbitrary a => Arbitrary (V.Vector a) where arbitrary = V.fromList <$> arbitrary
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

shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe a b | a == b = pure ()
             | otherwise = putStrLn $ show a ++ " /= " ++ show b


converts :: (NativeConverter a, Show a, Eq a) => a -> IO ()
converts a = a `shouldBe` fromNative (toNative a)

convertsExpression :: Expression -> IO ()
convertsExpression = converts

testAll :: IO ()
testAll = do
    -- quickCheck (mkConversionSpec :: Check ST)
    -- quickCheck (mkConversionSpec :: Check Expression)
    -- quickCheck (mkConversionSpec :: Check OutGraph)

    converts ("b" ::Binding)
    let sym = Local "b" :: ResolvedSymbol
    unless (sym == fromNative (toNative sym)) $ putStrLn "failed"
    converts (Local "b" :: ResolvedSymbol)
    converts (Sf (QualifiedBinding (nsRefFromList ["b", "c"]) "a") Nothing :: ResolvedSymbol)
    converts (Env 0 :: ResolvedSymbol)
    convertsExpression (Var (Local "b"))
    convertsExpression (Apply (Var (Sf (QualifiedBinding (nsRefFromList ["b", "c"]) "a") Nothing)) (Var (Local "b")))
    convertsExpression (Let "b" (Var (Env 0)) (Var (Local "b")))
    convertsExpression (Let ["b", "c"] (Var (Env 0)) (Var (Local "b")))
    convertsExpression (Lambda "a" (Var (Local "a")))
    convertsExpression (Lambda ["a", "b"] (Var (Local "a")))



    -- exitWith (ExitFailure 1)

{-|
Module      : $Header$
Description : Compatibility functions for eta and Java/Clojure with respects to ohua.
Copyright   : (c) Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

This source code is licensed under the terms described in the associated LICENSE.TXT file.
-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, MagicHash, TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds, BangPatterns, MultiWayIf #-}
module Ohua.Compat.JVM.Marshal where

import Java
import Java.ConversionUtils
import Lens.Micro
import Ohua.ALang.Lang
import Ohua.Types
import Control.Arrow ((***))
import qualified Data.HashMap.Strict as HM
import Ohua.LensClasses
import Unsafe.Coerce
import Ohua.DFGraph
import Ohua.Compile
import qualified Clojure
import qualified Clojure.Core as Clojure
import System.IO.Unsafe
import System.IO
import Ohua.Compat.JVM.ClojureST as ClST
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Data.Sequence as S
import Data.Foldable (toList)
import qualified Data.Map as M
import Ohua.Monad
import qualified Ohua.Util.Str as Str
import Data.Functor.Foldable
import Data.Functor.Compose


type CljExpr = Object

not_implemented :: a
not_implemented = error "This function is not (yet) implemented"

instance NativeConverter QualifiedBinding where
    type NativeType QualifiedBinding = JString
    toNative (QualifiedBinding ns name) =
        toNative $
        Str.toString $
        Str.intercalate "." (map unwrap $ unwrap ns) <> "/" <> (unwrap name)
    fromNative =
        either (error . Str.toString) expectQual . symbolFromString . fromNative
      where
        expectQual (Qual q) = q
        expectQual s = error $ "Expected qualified binding, got " ++ show s

data NExpr = NExpr @ohua.alang.Expr deriving Class

instance NativeConverter Expression where
    type NativeType Expression = NExpr
    fromNative nexpr =
        case ( safeDowncast nexpr
             , safeDowncast nexpr
             , safeDowncast nexpr
             , safeDowncast nexpr) of
            (Just v, _, _, _) -> Var (fromNative $ nVarExprValue v)
            (_, Just lam, _, _) ->
                Lambda
                    (fromNative $ nLambdaExprAssignment lam)
                    (fromNative $ nLambdaExprBody lam)
            (_, _, Just app, _) ->
                Apply
                    (fromNative $ nApplyExprFunction app)
                    (fromNative $ nApplyExprArgument app)
            (_, _, _, Just let_) ->
                Let
                    (fromNative $ nLetExprAssignment let_)
                    (fromNative $ nLetExprValue let_)
                    (fromNative $ nLetExprBody let_)
            _ -> error $ "unconvertable " ++ fromJava (toString nexpr)
    toNative (Let bnd val body) =
        superCast $ newLetExpr (toNative bnd) (toNative val) (toNative body)
    toNative (Lambda assign body) =
        superCast $ newLambdaExpr (toNative assign) (toNative body)
    toNative (Apply assign body) =
        superCast $ newApplyExpr (toNative assign) (toNative body)
    toNative (Var v) = superCast $ newVarExpr $ toNative v

data NLetExpr = NLetExpr @ohua.alang.Expr$Let deriving Class
type instance Inherits NLetExpr = '[NExpr]

foreign import java "@field assignment" nLetExprAssignment :: NLetExpr -> NAssignment
foreign import java "@field value" nLetExprValue :: NLetExpr -> NExpr
foreign import java "@field body" nLetExprBody :: NLetExpr -> NExpr
foreign import java "@new" newLetExpr :: NAssignment -> NExpr -> NExpr -> NLetExpr

data NApplyExpr = NApplyExpr @ohua.alang.Expr$Apply deriving Class
type instance Inherits NApplyExpr = '[NExpr]

foreign import java "@field function" nApplyExprFunction :: NApplyExpr -> NExpr
foreign import java "@field argument" nApplyExprArgument :: NApplyExpr -> NExpr
foreign import java "@new" newApplyExpr :: NExpr -> NExpr -> NApplyExpr

data NVarExpr = NVarExpr @ohua.alang.Expr$Var deriving Class
type instance Inherits NVarExpr = '[NExpr]

foreign import java "@field value" nVarExprValue :: NVarExpr -> NResolvedSymbol
foreign import java "@new" newVarExpr :: NResolvedSymbol -> NVarExpr

data NLambdaExpr = NLambdaExpr @ohua.alang.Expr$Lambda deriving Class
type instance Inherits NLambdaExpr = '[NExpr]

foreign import java "@field assignment" nLambdaExprAssignment :: NLambdaExpr -> NAssignment
foreign import java "@field body" nLambdaExprBody :: NLambdaExpr -> NExpr
foreign import java "@new" newLambdaExpr :: NAssignment -> NExpr -> NLambdaExpr

data NAssignment = NAssignment @ohua.alang.Assignment deriving (Class)
data NDirectAssignment = NDirectAssignment @ohua.alang.Assignment$Direct deriving (Class)
type instance Inherits NDirectAssignment = '[NAssignment]

foreign import java "@field binding" nDirectAssignmentBinding :: NDirectAssignment -> NBinding
foreign import java "@new" newDirectAssignment :: NBinding -> NDirectAssignment

data NDestructureAssignment = NDestructureAssignment @ohua.alang.Assignment$Destructure deriving (Class)
type instance Inherits NDestructureAssignment = '[NAssignment]

foreign import java "@field bindings" nDestructureAssignmentBindings :: NDestructureAssignment -> NBindingArr
foreign import java "@new" newDestructureAssignment :: NBindingArr -> NDestructureAssignment


instance NativeConverter Assignment where
    type NativeType Assignment = NAssignment
    fromNative assign =
        case (safeDowncast assign, safeDowncast assign) of
            (Just dir, _) -> Direct $ fromNative $ nDirectAssignmentBinding dir
            (_, Just destr) ->
                Destructure $
                map fromNative $
                fromJava (nDestructureAssignmentBindings destr :: NBindingArr)
            _ -> error $ "unconvertable " ++ fromJava (toString assign)
    toNative (Direct bnd) = superCast $ newDirectAssignment $ toNative bnd
    toNative (Destructure bnds) =
        superCast $ newDestructureAssignment $ toJava $ map toNative bnds


data NBinding = NBinding @ohua.types.Binding deriving (Class)

instance NativeConverter Binding where
    type NativeType Binding = NBinding
    fromNative = makeThrow . fromNative . nBindingValue
    toNative = mkNBinding . toNative . unwrap

foreign import java "@field value" nBindingValue :: NBinding -> JString
foreign import java "@static ohua.types.Binding.mk" mkNBinding :: JString -> NBinding

data NBindingArr = NBindingArr @ohua.types.Binding[] deriving (Class)

instance JArray NBinding NBindingArr

data NResolvedSymbol = NResolvedSymbol @ohua.alang.ResolvedSymbol deriving Class

instance NativeConverter ResolvedSymbol where
    type NativeType ResolvedSymbol = NResolvedSymbol
    fromNative nsym =
        case (safeDowncast nsym, safeDowncast nsym, safeDowncast nsym) of
            (Just v, _, _) -> Local $ fromNative $ nLocalBindingBinding v
            (_, Just v, _) ->
                Sf
                    (fromNative $ nSfBindingFnName v)
                    (fmap fromNative $ nSfBindingId v)
            (_, _, Just v) -> Env $ fromNative $ nEnvBindingId v
            _ -> error "unconvertable"
    toNative (Local l) = superCast $ newLocalBinding $ toNative l
    toNative (Sf name num) =
        superCast $
        maybe
            newSFBinding1
            (flip newSFBinding)
            (fmap (toJava . unwrap) num)
            (toNative name)
    toNative (Env n) = superCast $ newEnvBinding $ toJava $ unwrap n

data {-# CLASS "ohua.alang.ResolvedSymbol$Local" #-} NLocalBinding = NLocalBinding (Object# NLocalBinding) deriving Class

type instance Inherits NLocalBinding = '[NResolvedSymbol]

foreign import java "@field binding" nLocalBindingBinding :: NLocalBinding -> NBinding
foreign import java "@new" newLocalBinding :: NBinding -> NLocalBinding


data {-# CLASS "ohua.alang.ResolvedSymbol$Sf" #-} NSfBinding = NSfBinding (Object# NSfBinding) deriving Class

type instance Inherits NSfBinding = '[NResolvedSymbol]

foreign import java "@field fnName" nSfBindingFnName :: NSfBinding -> JString
foreign import java "@field fnId" nSfBindingId :: NSfBinding -> Maybe JInteger
foreign import java "@new" newSFBinding :: JString -> JInteger -> NSfBinding
foreign import java "@new" newSFBinding1 :: JString -> NSfBinding


data {-# CLASS "ohua.alang.ResolvedSymbol$Env" #-} NEnvBinding = NEnvBinding (Object# NEnvBinding) deriving Class

type instance Inherits NEnvBinding = '[NResolvedSymbol]

foreign import java "@field id" nEnvBindingId :: NEnvBinding -> JInteger
foreign import java "@new" newEnvBinding :: JInteger -> NEnvBinding


data {-# CLASS "ohua.graph.Target" #-} NTarget = NTarget (Object# NTarget) deriving Class

instance NativeConverter Target where
    type NativeType Target = NTarget
    toNative (Target id idx) = newNTarget (toNative id) (toJava idx)
    fromNative = not_implemented

foreign import java "@new" newNTarget :: JInteger -> JInteger -> NTarget

data {-# CLASS "ohua.graph.Graph" #-} NGraph envExpr = NGraph (Object# (NGraph envExpr)) deriving Class

instance NativeConverter a => NativeConverter (AbstractOutGraph a) where
    type NativeType (AbstractOutGraph a) = NGraph (NativeType a)
    toNative (OutGraph ops arcs _) =
        newNGraph (toJava $ map toNative ops) (toJava $ map toNative arcs)
    fromNative = not_implemented

foreign import java "@new" newNGraph :: NOperatorArr -> NArcArr a -> NGraph a

data {-# CLASS "ohua.graph.Operator" #-} NOperator = NOperator (Object# NOperator) deriving Class

instance NativeConverter Operator where
    type NativeType Operator = NOperator
    toNative (Operator id t) = newNOperator (toNative id) (toNative t)
    fromNative = not_implemented
    -- fromNative op = Operator (fromNative (nOperatorId op)) (fromNative (nOperatorType op))

foreign import java "@new" newNOperator :: JInteger -> JString -> NOperator
-- foreign import java "@field id" nOperatorId :: NOperator -> JInteger
-- foreign import java "@field type" nOperatorType :: NOperator -> JString

data {-# CLASS "ohua.graph.Operator[]" #-} NOperatorArr = NOperatorArr (Object# NOperatorArr) deriving Class

instance JArray NOperator NOperatorArr

data {-# CLASS "ohua.graph.Source" #-} NSource a = NSource (Object# (NSource a)) deriving Class

instance NativeConverter a => NativeConverter (Source a) where
    type NativeType (Source a) = NSource (NativeType a)
    toNative (LocalSource t) = superCast $ newNLocalSource $ toNative t
    toNative (EnvSource e) = superCast $ newNEnvSource $ toNative e
    fromNative s = not_implemented
        -- case (safeDowncast s, safeDowncast s) of
        --     (Just loc, _) -> LocalSource $ fromNative $ nLocalSourceTarget loc
        --     (_, Just env) -> EnvSource $ fromNative $ nEnvSourceHostExpr env
        --     _ -> error "Could not coerce source"


data {-# CLASS "ohua.graph.Source$Local" #-} NLocalSource a = NLocalSource (Object# (NLocalSource a)) deriving Class

type instance Inherits (NLocalSource a) = '[NSource a]
foreign import java "@new" newNLocalSource :: NTarget -> NLocalSource a
-- foreign import java  "@field target" nLocalSourceTarget :: NLocalSource a -> NTarget

data {-# CLASS "ohua.graph.Source$Env" #-} NEnvSource a = NEnvSource (Object# (NEnvSource a)) deriving Class

type instance Inherits (NEnvSource a) = '[NSource a]
foreign import java "@new" newNEnvSource :: (a <: Object) => a -> NEnvSource a
-- foreign import java "@field hostExpr" nEnvSourceHostExpr :: NEnvSource a -> a

data {-# CLASS "ohua.graph.Arc" #-} NArc a = NArc (Object# (NArc a)) deriving Class

instance NativeConverter a => NativeConverter (Arc a) where
    type NativeType (Arc a) = NArc (NativeType a)
    toNative (Arc target source) = newNArc (toNative target) (toNative source)
    fromNative = not_implemented

foreign import java "@new" newNArc :: (a <: Object) => NTarget -> NSource a -> NArc a

data {-# CLASS "ohua.graph.Arc[]" #-} NArcArr a = NArcArr (Object# (NArcArr a)) deriving Class

instance JArray (NArc a) (NArcArr a)

instance NativeConverter FnId where
    type NativeType FnId = JInteger
    fromNative = makeThrow . fromJava
    toNative = toJava . unwrap

instance NativeConverter HostExpr where
    type NativeType HostExpr = JInteger
    fromNative = makeThrow . fromJava
    toNative = toJava . unwrap

instance NativeConverter Object where
    type NativeType Object = Object
    fromNative = id
    toNative = id

instance NativeConverter (NLazy a) where
    type NativeType (NLazy a) = NLazy a
    fromNative = id
    toNative = id

data {-# CLASS "ohua.util.Lazy" #-} NLazy a = NLazy (Object# (NLazy a)) deriving Class

data {-# CLASS "ohua.util.Lazy[]" #-} NLazyArray a = NLazyArray (Object# (NLazyArray a)) deriving Class

instance JArray (NLazy a) (NLazyArray a)

data Algo = Algo !Expression !(S.Seq (NLazy Object))

data {-# CLASS "ohua.Algo" #-} NAlgo = NAlgo (Object# NAlgo) deriving Class

foreign import java "@new" newNAlgo :: NExpr -> NLazyArray Object -> NAlgo
foreign import java "@field code" nAlgoCode :: NAlgo -> NExpr
foreign import java "@field envExprs" nAlgoEnvExprs :: NAlgo -> NLazyArray Object

instance NativeConverter Algo where
    type NativeType Algo = NAlgo
    toNative (Algo code exprs) =
        newNAlgo (toNative code) (toJava $ toList exprs)
    fromNative o =
        Algo
            (fromNative $ nAlgoCode o)
            (S.fromList $ fromJava $ nAlgoEnvExprs o)


instance NativeConverter LogLevel where
    type NativeType LogLevel = Object
    fromNative l
        | Clojure.eq kwDebug l = LevelDebug
        | Clojure.eq kwInfo l = LevelInfo
        | Clojure.eq kwWarn l = LevelWarn
        | Clojure.eq kwError l = LevelError
        | Clojure.isKw l =
            case Clojure.namespace l of
                Nothing -> LevelOther $ T.pack $ Str.toString $ Clojure.name l
                _ -> error "Expected non-namespaced keyword for logging level."
        | otherwise =
            error "Unexpected type for logging level, expected keyword."
    toNative LevelDebug = kwDebug
    toNative LevelInfo = kwInfo
    toNative LevelWarn = kwWarn
    toNative LevelError = kwError
    toNative (LevelOther l) = Clojure.keyword (T.unpack l)


kwDebug, kwInfo, kwWarn, kwError :: Object
kwDebug = Clojure.keyword "debug"
kwInfo = Clojure.keyword "info"
kwWarn = Clojure.keyword "warning"
kwError = Clojure.keyword "error"

mkSym :: ClST.Symbol -> Object
mkSym sym =
    unsafePerformJavaWith (Clojure.coreVar "symbol") $
    case sym of
        Symbol Nothing name -> Clojure.invoke1 $ convert name
        Symbol (Just ns) name -> Clojure.invoke2 (convert ns) (convert name)
  where
    convert = superCast . (toNative :: Str.Str -> JString)

instance NativeConverter (AnnST Clojure.Meta) where
    type NativeType (AnnST Clojure.Meta) = Object
    fromNative =
        ana $ \obj ->
            let asSeq =
                    fromJava $ (unsafeCast :: Object -> Collection Object) obj
             in Compose $
                Annotated (Clojure.meta obj) $
                if | Clojure.isSeq obj -> FormF asSeq
                   | Clojure.isSymbol obj ->
                       SymF $ Symbol (Clojure.namespace obj) (Clojure.name obj)
                   | Clojure.isVector obj -> VecF asSeq
                   | otherwise -> LitF obj
    toNative =
        cata $ \(Compose (Annotated ann val)) ->
            flip Clojure.withMeta ann $
            case val of
                FormF vals ->
                    Clojure.asSeq $
                    (superCast :: Collection Object -> Object) $ toJava vals
                SymF sym -> mkSym sym
                VecF v ->
                    Clojure.vector $
                    (superCast :: Collection Object -> Object) $ toJava v
                LitF o -> o

instance ToEnvExpr (AnnST Clojure.Meta) where
    toEnvExpr = toNative


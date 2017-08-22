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
{-# LANGUAGE TypeFamilies, DataKinds #-}
module Ohua.Compat where

import Java
import Lens.Micro
import Ohua.ALang.Lang
import Ohua.IR
import Ohua.Types
import Control.Arrow ((***))
import qualified Data.HashMap.Strict as HM
import Ohua.LensClasses
import Unsafe.Coerce
import Ohua.DFGraph
import Ohua.Compile


instance Show Object where show = fromJString . toString

class NativeConverter a where
    type NativeType a
    fromNative :: NativeType a -> a
    toNative :: a -> NativeType a


type CljExpr = Object

not_implemented :: a
not_implemented = error "This function is not (yet) implemented"

data {-# CLASS "com.ohua.alang.Expr" #-} NExpr = NExpr (Object# NExpr) deriving (Class)


instance NativeConverter Expression where
    type NativeType Expression = NExpr
    fromNative nexpr =
        case (safeDowncast nexpr, safeDowncast nexpr, safeDowncast nexpr, safeDowncast nexpr) of
            (Just v, _, _, _) -> Var (fromNative $ nVarExprValue v)
            (_, Just lam, _, _) ->
                Lambda (fromNative $ nLambdaExprAssignment lam) (fromNative $ nLambdaExprBody lam)
            (_, _, Just app, _) ->
                Apply (fromNative $ nApplyExprFunction app) (fromNative $ nApplyExprArgument app)
            (_, _, _, Just let_) ->
                Let (fromNative $ nLetExprAssignment let_)
                    (fromNative $ nLetExprValue let_)
                    (fromNative $ nLetExprBody let_)
            _ -> error "unconvertable"
    toNative = not_implemented

data {-# CLASS "ohua.alang.Expr$Let" #-} NLetExpr = NLetExpr (Object# NLetExpr) deriving (Class)

foreign import java "@field Assignment" nLetExprAssignment :: NLetExpr -> NAssignment
foreign import java "@field value" nLetExprValue :: NLetExpr -> NExpr
foreign import java "@field body" nLetExprBody :: NLetExpr -> NExpr

data {-# CLASS "ohua.alang.Expr$Apply" #-} NApplyExpr = NApplyExpr (Object# NApplyExpr) deriving (Class)

foreign import java "@field function" nApplyExprFunction :: NApplyExpr -> NExpr
foreign import java "@field argument" nApplyExprArgument :: NApplyExpr -> NExpr

data {-# CLASS "ohua.alang.Expr$Var" #-} NVarExpr = NVarExpr (Object# NVarExpr) deriving (Class)

foreign import java "@field value" nVarExprValue :: NVarExpr -> NResolvedSymbol

data {-# CLASS "ohua.alang.Expr$Lambda" #-} NLambdaExpr = NLambdaExpr (Object# NLambdaExpr) deriving (Class)

foreign import java "@field Assignment" nLambdaExprAssignment :: NLambdaExpr -> NAssignment

instance NativeConverter Assignment where
    type NativeType Assignment = NAssignment
    fromNative assign =
        case (safeDowncast assign, safeDowncast assign) of
            (Just dir, _) -> Direct $ fromNative $ nDirectAssignmentBinding dir
            (_, Just destr) -> Destructure $ map fromNative $ fromJava (destr :: NBindingArr)
            _ -> error "unconvertable"
    toNative = not_implemented
            
foreign import java "@field body" nLambdaExprBody :: NLambdaExpr -> NExpr

data {-# CLASS "ohua.alang.Assignment" #-} NAssignment = NAssignment (Object# NAssignment) deriving (Class)
data {-# CLASS "ohua.alang.Assignment$Direct" #-} NDirectAssignment = NDirectAssignment (Object# NDirectAssignment) deriving (Class)

foreign import java "@field binding" nDirectAssignmentBinding :: NDirectAssignment -> NBinding

data {-# CLASS "ohua.alang.Assignment$Destructure" #-} NDestructureAssignment = NDestructureAssignment (Object# NDestructureAssignment) deriving (Class)

foreign import java "@field bindings" nDestructureAssignmentBindings :: NDestructureAssignment -> NBindingArr

data {-# CLASS "ohua.types.Binding" #-} NBinding = NBinding (Object# NBinding) deriving (Class)

instance NativeConverter Binding where
    type NativeType Binding = NBinding
    fromNative nbind = Binding (fromJava $ nBindingValue nbind)
    toNative = not_implemented

foreign import java "@field value" nBindingValue :: NBinding -> JString

data {-# CLASS "ohua.types.Binding[]" #-} NBindingArr = NBindingArr (Object# NBindingArr) deriving (Class)

instance JArray NBinding NBindingArr

data {-# CLASS "ohua.alang.ResolvedSymbol" #-} NResolvedSymbol = NResolvedSymbol (Object# NResolvedSymbol) deriving Class

instance NativeConverter ResolvedSymbol where
    type NativeType ResolvedSymbol = NResolvedSymbol

    fromNative nsym =
        case (safeDowncast nsym, safeDowncast nsym, safeDowncast nsym) of
            (Just v, _, _) -> Local $ fromNative $ nLocalBindingBinding v
            (_, Just v, _) -> Sf (fromNative $ nSfBindingFnName v) (fmap fromNative $ nSfBindingId v)
            (_, _, Just v) -> Env $ fromNative $ nEnvBindingId v
            _ -> error "unconvertable"
    toNative = not_implemented

data {-# CLASS "ohua.alang.ResolvedSymbol$Local" #-} NLocalBinding = NLocalBinding (Object# NLocalBinding) deriving Class

foreign import java "@field binding" nLocalBindingBinding :: NLocalBinding -> NBinding


data {-# CLASS "ohua.alang.ResolvedSymbol$Sf" #-} NSfBinding = NSfBinding (Object# NSfBinding) deriving Class

foreign import java "@field fnName" nSfBindingFnName :: NSfBinding -> NFnName
foreign import java "@field fnId" nSfBindingId :: NSfBinding -> Maybe JInteger

data {-# CLASS "ohua.alang.ResolvedSymbol$Env" #-} NEnvBinding = NEnvBinding (Object# NEnvBinding) deriving Class

foreign import java "@field id" nEnvBindingId :: NEnvBinding -> JInteger



data {-# CLASS "ohua.graph.Target" #-} NTarget = NTarget (Object# NTarget) deriving Class

instance NativeConverter Target where
    type NativeType Target = NTarget
    toNative (Target id idx) = newNTarget (toNative id) (toJava idx)
    fromNative = not_implemented

foreign import java "@new" newNTarget :: JInteger -> JInteger -> NTarget

data {-# CLASS "ohua.graph.Graph" #-} NGraph = NGraph (Object# NGraph) deriving Class

instance NativeConverter OutGraph where
    type NativeType OutGraph = NGraph
    toNative (OutGraph ops arcs) = newNGraph (toJava $ map toNative ops) (toJava $ map toNative arcs)
    fromNative = not_implemented

foreign import java "@new" newNGraph :: NOperatorArr -> NArcArr -> NGraph

data {-# CLASS "ohua.graph.Operator" #-} NOperator = NOperator (Object# NOperator) deriving Class

instance NativeConverter Operator where
    type NativeType Operator = NOperator
    toNative (Operator id t) = newNOperator (toNative id) (toNative t)
    fromNative = not_implemented

foreign import java "@new" newNOperator :: JInteger -> NFnName -> NOperator

data {-# CLASS "ohua.graph.Operator[]" #-} NOperatorArr = NOperatorArr (Object# NOperatorArr) deriving Class

instance JArray NOperator NOperatorArr

data {-# CLASS "ohua.graph.Source" #-} NSource = NSource (Object# NSource) deriving Class

instance NativeConverter Source where
    type NativeType Source = NSource
    toNative (LocalSource t) = superCast $ newNLocalSource $ toNative t
    toNative (EnvSource e) = superCast $ newNEnvSource $ toNative e
    fromNative = not_implemented


data {-# CLASS "ohua.graph.Source$Local" #-} NLocalSource = NLocalSource (Object# NLocalSource) deriving Class
    
type instance Inherits NLocalSource = '[NSource]
foreign import java "@new" newNLocalSource :: NTarget -> NLocalSource

data {-# CLASS "ohua.graph.Source$Env" #-} NEnvSource = NEnvSource (Object# NEnvSource) deriving Class

type instance Inherits NEnvSource = '[NSource]
foreign import java "@new" newNEnvSource :: JInteger -> NEnvSource

data {-# CLASS "ohua.graph.Arc" #-} NArc = NArc (Object# NArc) deriving Class

instance NativeConverter Arc where
    type NativeType Arc = NArc
    toNative (Arc target source) = newNArc (toNative target) (toNative source)
    fromNative = not_implemented

foreign import java "@new" newNArc :: NTarget -> NSource -> NArc

data {-# CLASS "ohua.graph.Arc[]" #-} NArcArr = NArcArr (Object# NArcArr) deriving Class

instance JArray NArc NArcArr

data {-# CLASS "ohua.types.FnName" #-} NFnName = NFnName (Object# NFnName) deriving Class

instance NativeConverter FnName where
    type NativeType FnName = NFnName
    fromNative fnName = FnName (fromJava $ nFnNameName fnName) (fromJava $ nFnNameNamespace fnName)
    toNative = not_implemented

foreign import java "@field name" nFnNameName :: NFnName -> JString
foreign import java "@field namespace" nFnNameNamespace :: NFnName -> JString

instance NativeConverter FnId where
    type NativeType FnId = JInteger
    fromNative = FnId . fromJava
    toNative = toJava . unIRFnId

instance NativeConverter HostExpr where
    type NativeType HostExpr = JInteger
    fromNative = HostExpr . fromJava
    toNative = toJava . unwrapHostExpr


data {-# CLASS "ohua.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class


nativeCompile :: NExpr -> Java NCompiler NGraph
nativeCompile = return . toNative . either error id . compile . fromNative


foreign export java "@static ohua.Compiler.compile" nativeCompile :: NExpr -> Java NCompiler NGraph

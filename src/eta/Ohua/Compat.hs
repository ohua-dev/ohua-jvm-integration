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

data {-# CLASS "clojure.lang.Symbol" #-} Symbol = Symbol (Object# Symbol) deriving Class

foreign import java "@static com.ohua.util.Eta.createSymbol" createSymbol :: String -> String -> Symbol
foreign import java "@static com.ohua.util.Eta.createSymbol" createSymbol1 :: String -> Symbol

foreign import java "getName" symGetName :: Symbol -> String
foreign import java "getNamespace" symGetNamespace :: Symbol -> Maybe String

data {-# CLASS "clojure.lang.Keyword" #-} Keyword = Keyword (Object# Keyword) deriving (Class, Eq)
foreign import java "@static clojure.lang.Keyword.find" createKeyword :: String -> Keyword

instance Show Object where show = fromJString . toString

class NativeConverter a where
    type NativeType a
    fromNative :: NativeType a -> a
    toNative :: a -> NativeType a


type CljExpr = Object



data {-# CLASS "com.ohua.util.Expr" #-} NExpr = NExpr (Object# NExpr) deriving (Class)
data {-# CLASS "com.ohua.util.Expr$Let" #-} NLetExpr = NLetExpr (Object# NLetExpr) deriving (Class)
data {-# CLASS "com.ohua.util.Expr$Apply" #-} NApplyExpr = NApplyExpr (Object# NApplyExpr) deriving (Class)
data {-# CLASS "com.ohua.util.Expr$Var" #-} NVarExpr = NVarExpr (Object# NVarExpr) deriving (Class)
data {-# CLASS "com.ohua.util.Expr$Lambda" #-} NLambdaExpr = NLambdaExpr (Object# NLambdaExpr) deriving (Class)

data {-# CLASS "com.ohua.util.Assignment" #-} NAssignment = NAssignment (Object# NAssignment) deriving (Class)
data {-# CLASS "com.ohua.util.Assignment$Direct" #-} NDirectAssignment = NDirectAssignment (Object# NDirectAssignment) deriving (Class)
data {-# CLASS "com.ohua.util.Assignment$Destructure" #-} NDestructureAssignment = NDestructureAssignment (Object# NDestructureAssignment) deriving (Class)

data {-# CLASS "com.ohua.util.Binding" #-} NBinding = NBinding (Object# NBinding) deriving (Class)
data {-# CLASS "com.ohua.util.Binding[]" #-} NBindingArr = NBindingArr (Object# NBindingArr) deriving (Class)
data {-# CLASS "com.ohua.util.ResolvedSymbol" #-} NResolvedSymbol = NResolvedSymbol (Object# NResolvedSymbol) deriving Class
data {-# CLASS "com.ohua.util.ResolvedSymbol$Local" #-} NLocalBinding = NLocalBinding (Object# NLocalBinding) deriving Class
data {-# CLASS "com.ohua.util.ResolvedSymbol$Sf" #-} NSfBinding = NSfBinding (Object# NSfBinding) deriving Class
data {-# CLASS "com.ohua.util.ResolvedSymbol$Algo" #-} NAlgoBinding = NAlgoBinding (Object# NAlgoBinding) deriving Class
data {-# CLASS "com.ohua.util.ResolvedSymbol$Env" #-} NEnvBinding = NEnvBinding (Object# NEnvBinding) deriving Class

instance JArray NBinding NBindingArr

foreign import java "@field Assignment" nLetExprAssignment :: NLetExpr -> NAssignment
foreign import java "@field value" nLetExprValue :: NLetExpr -> NExpr
foreign import java "@field body" nLetExprBody :: NLetExpr -> NExpr

foreign import java "@field Assignment" nLambdaExprAssignment :: NLambdaExpr -> NAssignment
foreign import java "@field body" nLambdaExprBody :: NLambdaExpr -> NExpr

foreign import java "@field function" nApplyExprFunction :: NApplyExpr -> NExpr
foreign import java "@field argument" nApplyExprArgument :: NApplyExpr -> NExpr

foreign import java "@field value" nVarExprValue :: NVarExpr -> NResolvedSymbol


foreign import java "@field binding" nDirectAssignmentBinding :: NDirectAssignment -> NBinding
foreign import java "@field bindings" nDestructureAssignmentBindings :: NDestructureAssignment -> NBindingArr

foreign import java "@field value" nBindingValue :: NBinding -> JString

foreign import java "@field binding" nLocalBindingBinding :: NLocalBinding -> NBinding

foreign import java "@field fnName" nSfBindingFnName :: NSfBinding -> NFnName
foreign import java "@field fnId" nSfBindingId :: NSfBinding -> Maybe JInteger

foreign import java "@field algoName" nAlgoBindingAlgoName :: NAlgoBinding -> NFnName

foreign import java "@field id" nEnvBindingId :: NEnvBinding -> JInteger

data {-# CLASS  "com.ohua.util.FnName" #-} NFnName = NFnName (Object# NFnName) deriving Class

foreign import java "@field name" nFnNameName :: NFnName -> JString
foreign import java "@field namespace" nFnNameNamespace :: NFnName -> JString

data {-# CLASS "com.ohua.ir.IRFunc" #-} NativeIRFunc = NativeIRFunc (Object# NativeIRFunc) deriving Class

foreign import java "@new" mkNativeIRFunc :: Object -> Object -> Object -> Object -> NativeIRFunc
foreign import java "@field id" getNativeIRFuncId :: NativeIRFunc -> Object
foreign import java "@field args" getNativeIRFuncArgs :: NativeIRFunc -> Object
foreign import java "@field name" getNativeIRFuncName :: NativeIRFunc -> Object
foreign import java "@field return" getNativeIRFuncReturn :: NativeIRFunc -> Object

instance NativeConverter IRFn where
    type NativeType IRFn = NativeIRFunc
    fromNative func = IRFn
        (fromNative $ unsafeCast $ getNativeIRFuncId func)
        (fromNative $ unsafeCast $ getNativeIRFuncName func)
        (fromNative $ unsafeCast $ getNativeIRFuncArgs func)
        (fromNative $ unsafeCast $ getNativeIRFuncReturn func)
    toNative (IRFn id_ name_ args ret) = mkNativeIRFunc
        (superCast $ toNative id_)
        (superCast $ toNative name_)
        (superCast $ toNative args)
        (superCast $ toNative ret)


instance NativeConverter IRGraph where
    type NativeType IRGraph = Collection (NativeType IRFn)
    fromNative = map fromNative . fromJava
    toNative = toJava . map toNative


opIdKW = createKeyword "op-id"
typeKW = createKeyword "type"
outVarKW = createKeyword "out-var"

instance NativeConverter CtxtFrame where
    type NativeType CtxtFrame = Map Keyword Object
    fromNative nm = CtxtFrame (fromNative $ unsafeCast frameType) (fromNative $ unsafeCast opid) (fromJava $ (unsafeCast outVar :: JInteger))
      where
        m = fromJava nm :: [(Keyword, Object)]
        Just opid = lookup opIdKW m
        Just frameType = lookup typeKW m
        Just outVar = lookup outVarKW m
    toNative frame = toJava
        [ (opIdKW, superCast $ toNative $ frame^.sourceFn :: Object)
        , (typeKW, superCast $ toNative $ frame^.frameType)
        , (outVarKW, superCast $ (toJava $ frame^.outVar :: JInteger))
        ]

instance NativeConverter CtxtStack where
    type NativeType CtxtStack = List (NativeType CtxtFrame)
    toNative = toJava . map toNative
    fromNative = map fromNative . fromJava


instance NativeConverter CtxtMap where
    type NativeType CtxtMap = Map JInteger (NativeType CtxtStack)
    fromNative = HM.fromList . map (fromNative *** fromNative) . fromJava
    toNative = toJava . map (toNative *** toNative) . HM.toList

instance NativeConverter Argument where
    type NativeType Argument = Object
    fromNative arg =
        case safeDowncast arg of
            Just sym -> ABinding $ fromNative sym
            Nothing -> AEnv $ unsafeCoerce arg
    toNative (AEnv o) = unsafeCoerce o
    toNative (ABinding bnd) = superCast $ toNative bnd

instance NativeConverter Arguments where
    type NativeType Arguments = List (NativeType Argument)
    fromNative = map fromNative . fromJava
    toNative = toJava . map toNative

instance NativeConverter FnReturn where
    type NativeType FnReturn = Object
    fromNative ret =
        case safeDowncast ret of
            Just sym -> Direct $ fromNative sym
            Nothing -> Destructure $ map fromNative (fromJava $ (unsafeCast ret :: Collection Symbol))
    toNative (Direct bnd) = superCast $ toNative bnd
    toNative (Destructure l) = superCast $ (toJava $ map toNative l :: List Symbol)

instance NativeConverter IRFnId where
    type NativeType IRFnId = JInteger
    fromNative = FnId . fromJava
    toNative = toJava . unIRFnId

instance NativeConverter FnName where
    type NativeType FnName = Symbol
    fromNative sym = maybe (error "Symbol for fn name must be fully qualified") (`FnName` symGetName sym) (symGetNamespace sym)
    toNative n = createSymbol (n^.namespace) (n^.name)

instance NativeConverter Binding where
    type NativeType Binding = Symbol
    toNative = createSymbol1 . unBinding
    fromNative sym = maybe (Binding $ symGetName sym) (const $ error "Symbol for binding must not be qualified") (symGetNamespace sym)

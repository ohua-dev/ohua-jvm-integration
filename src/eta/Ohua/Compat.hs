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

data {-# CLASS "com.ohua.util.Expr$Let" #-} NLetExpr = NLetExpr (Object# NLetExpr) deriving (Class)

foreign import java "@field Assignment" nLetExprAssignment :: NLetExpr -> NAssignment
foreign import java "@field value" nLetExprValue :: NLetExpr -> NExpr
foreign import java "@field body" nLetExprBody :: NLetExpr -> NExpr

data {-# CLASS "com.ohua.util.Expr$Apply" #-} NApplyExpr = NApplyExpr (Object# NApplyExpr) deriving (Class)

foreign import java "@field function" nApplyExprFunction :: NApplyExpr -> NExpr
foreign import java "@field argument" nApplyExprArgument :: NApplyExpr -> NExpr

data {-# CLASS "com.ohua.util.Expr$Var" #-} NVarExpr = NVarExpr (Object# NVarExpr) deriving (Class)

foreign import java "@field value" nVarExprValue :: NVarExpr -> NResolvedSymbol

data {-# CLASS "com.ohua.util.Expr$Lambda" #-} NLambdaExpr = NLambdaExpr (Object# NLambdaExpr) deriving (Class)

foreign import java "@field Assignment" nLambdaExprAssignment :: NLambdaExpr -> NAssignment

instance NativeConverter Assignment where
    type NativeType Assignment = NAssignment
    fromNative assign =
        case (safeDowncast assign, safeDowncast assign) of
            (Just dir, _) -> Direct $ fromNative $ nDirectAssignmentBinding dir
            (_, Just destr) -> Destructure $ map fromNative $ fromJava (destr :: NBindingArr)
            _ -> error "unconvertable"

foreign import java "@field body" nLambdaExprBody :: NLambdaExpr -> NExpr

data {-# CLASS "com.ohua.util.Assignment" #-} NAssignment = NAssignment (Object# NAssignment) deriving (Class)
data {-# CLASS "com.ohua.util.Assignment$Direct" #-} NDirectAssignment = NDirectAssignment (Object# NDirectAssignment) deriving (Class)

foreign import java "@field binding" nDirectAssignmentBinding :: NDirectAssignment -> NBinding

data {-# CLASS "com.ohua.util.Assignment$Destructure" #-} NDestructureAssignment = NDestructureAssignment (Object# NDestructureAssignment) deriving (Class)

foreign import java "@field bindings" nDestructureAssignmentBindings :: NDestructureAssignment -> NBindingArr

data {-# CLASS "com.ohua.util.Binding" #-} NBinding = NBinding (Object# NBinding) deriving (Class)

instance NativeConverter Binding where
    type NativeType Binding = NBinding
    fromNative nbind = Binding (fromJava $ nBindingValue nbind)

foreign import java "@field value" nBindingValue :: NBinding -> JString

data {-# CLASS "com.ohua.util.Binding[]" #-} NBindingArr = NBindingArr (Object# NBindingArr) deriving (Class)

instance JArray NBinding NBindingArr

data {-# CLASS "com.ohua.util.ResolvedSymbol" #-} NResolvedSymbol = NResolvedSymbol (Object# NResolvedSymbol) deriving Class

instance NativeConverter ResolvedSymbol where
    type NativeType ResolvedSymbol = NResolvedSymbol

    fromNative nsym =
        case (safeDowncast nsym, safeDowncast nsym, safeDowncast nsym, safeDowncast nsym) of
            (Just v, _, _, _) -> Local $ fromNative $ nLocalBindingBinding v
            (_, Just v, _, _) -> Sf (fromNative $ nSfBindingFnName v) (fmap fromNative $ nSfBindingId v)
            (_, _, Just v, _) -> Algo (fromNative $ nAlgoBindingAlgoName v)
            (_, _, _, Just v) -> Env $ fromNative $ nEnvBindingId v
            _ -> error "unconvertable"

data {-# CLASS "com.ohua.util.ResolvedSymbol$Local" #-} NLocalBinding = NLocalBinding (Object# NLocalBinding) deriving Class

foreign import java "@field binding" nLocalBindingBinding :: NLocalBinding -> NBinding


data {-# CLASS "com.ohua.util.ResolvedSymbol$Sf" #-} NSfBinding = NSfBinding (Object# NSfBinding) deriving Class

foreign import java "@field fnName" nSfBindingFnName :: NSfBinding -> NFnName
foreign import java "@field fnId" nSfBindingId :: NSfBinding -> Maybe JInteger

data {-# CLASS "com.ohua.util.ResolvedSymbol$Env" #-} NEnvBinding = NEnvBinding (Object# NEnvBinding) deriving Class

foreign import java "@field id" nEnvBindingId :: NEnvBinding -> JInteger



data {-# CLASS "com.ohua.util.Target" #-} NTarget = NTarget (Object# NTarget) deriving Class

instance NativeConverter Target where
    type NativeType Target = NTarget
    toNative (Target id idx) = newNTarget (toNative id) (toJava idx)

foreign import java "@new" newNTarget :: JInteger -> JInteger -> NTarget

data {-# CLASS "com.ohua.util.Graph" #-} NGraph = NGraph (Object# NGraph) deriving Class

instance NativeConverter OutGraph where
    type NativeType OutGraph = NGraph
    toNative (OutGraph ops arcs) = newNGraph (toJava $ map toNative ops) (toJava $ map toNative arcs)

foreign import java "@new" newNGraph :: NOperatorArr -> NArcArr -> NGraph

data {-# CLASS "com.ohua.util.Operator" #-} NOperator = NOperator (Object# NOperator) deriving Class

instance NativeConverter Operator where
    type NativeType Operator = NOperator
    toNative (Operator id type) = newNOperator (toNative id) (toNative type)

foreign import java "@new" newNOperator :: JInteger -> JString -> NOperator

data {-# CLASS "com.ohua.util.Operator[]" #-} NOperatorArr = NOperatorArr (Object# NOperatorArr) deriving Class

instance JArray NOperator NOperatorArr

data {-# CLASS "com.ohua.util.Source" #-} NSource = NSource (Object# NSource) deriving Class

instance NativeConverter Source where
    type NativeType Source = NSource
    toNative (LocalSource t) = superCast $ newNLocalSource $ toNative t
    toNative (EnvSource e) = superCast $ newNEnvSource $ toNative e

data {-# CLASS "com.ohua.util.Source$Local" #-} NLocalSource = NLocalSource (Object# NLocalSource) deriving Class

foreign import java "@new" newNLocalSource :: NTarget -> NLocalSource

data {-# CLASS "com.ohua.util.Source$Env" #-} NEnvSource = NEnvSource (Object# NEnvSource) deriving Class

foreign import java "@new" newNEnvSource :: JInteger -> NEnvSource

data {-# CLASS "com.ohua.util.Arc" #-} NArc = NArc (Object# NArc) deriving Class

instance NativeConverter Arc where
    type NativeType Arc = NArc
    toNative (Arc source target) = newNArc (toNative source) (toNative target)

foreign import java "@new" newNArc :: NSource -> NTarget -> NArc

data {-# CLASS "com.ohua.util.Arc[]" #-} NArcArr = NArcArr (Object# NArcArr) deriving Class

instance JArray NArc NArcArr

data {-# CLASS  "com.ohua.util.FnName" #-} NFnName = NFnName (Object# NFnName) deriving Class

instance NativeConverter FnName where
    type NativeType FnName = NFnName
    fromNative fnName = FnName (fromJava $ nFnNameName fnName) (fromJava $ nFnNameNamespace fnName)

foreign import java "@field name" nFnNameName :: NFnName -> JString
foreign import java "@field namespace" nFnNameNamespace :: NFnName -> JString

-- data {-# CLASS "com.ohua.ir.IRFunc" #-} NativeIRFunc = NativeIRFunc (Object# NativeIRFunc) deriving Class

-- foreign import java "@new" mkNativeIRFunc :: Object -> Object -> Object -> Object -> NativeIRFunc
-- foreign import java "@field id" getNativeIRFuncId :: NativeIRFunc -> Object
-- foreign import java "@field args" getNativeIRFuncArgs :: NativeIRFunc -> Object
-- foreign import java "@field name" getNativeIRFuncName :: NativeIRFunc -> Object
-- foreign import java "@field return" getNativeIRFuncReturn :: NativeIRFunc -> Object

-- instance NativeConverter IRFn where
--     type NativeType IRFn = NativeIRFunc
--     fromNative func = IRFn
--         (fromNative $ unsafeCast $ getNativeIRFuncId func)
--         (fromNative $ unsafeCast $ getNativeIRFuncName func)
--         (fromNative $ unsafeCast $ getNativeIRFuncArgs func)
--         (fromNative $ unsafeCast $ getNativeIRFuncReturn func)
--     toNative (IRFn id_ name_ args ret) = mkNativeIRFunc
--         (superCast $ toNative id_)
--         (superCast $ toNative name_)
--         (superCast $ toNative args)
--         (superCast $ toNative ret)


-- instance NativeConverter IRGraph where
--     type NativeType IRGraph = Collection (NativeType IRFn)
--     fromNative = map fromNative . fromJava
--     toNative = toJava . map toNative


-- opIdKW = createKeyword "op-id"
-- typeKW = createKeyword "type"
-- outVarKW = createKeyword "out-var"

-- instance NativeConverter CtxtFrame where
--     type NativeType CtxtFrame = Map Keyword Object
--     fromNative nm = CtxtFrame (fromNative $ unsafeCast frameType) (fromNative $ unsafeCast opid) (fromJava $ (unsafeCast outVar :: JInteger))
--       where
--         m = fromJava nm :: [(Keyword, Object)]
--         Just opid = lookup opIdKW m
--         Just frameType = lookup typeKW m
--         Just outVar = lookup outVarKW m
--     toNative frame = toJava
--         [ (opIdKW, superCast $ toNative $ frame^.sourceFn :: Object)
--         , (typeKW, superCast $ toNative $ frame^.frameType)
--         , (outVarKW, superCast $ (toJava $ frame^.outVar :: JInteger))
--         ]

-- instance NativeConverter CtxtStack where
--     type NativeType CtxtStack = List (NativeType CtxtFrame)
--     toNative = toJava . map toNative
--     fromNative = map fromNative . fromJava


-- instance NativeConverter CtxtMap where
--     type NativeType CtxtMap = Map JInteger (NativeType CtxtStack)
--     fromNative = HM.fromList . map (fromNative *** fromNative) . fromJava
--     toNative = toJava . map (toNative *** toNative) . HM.toList

-- instance NativeConverter Argument where
--     type NativeType Argument = Object
--     fromNative arg =
--         case safeDowncast arg of
--             Just sym -> ABinding $ fromNative sym
--             Nothing -> AEnv $ unsafeCoerce arg
--     toNative (AEnv o) = unsafeCoerce o
--     toNative (ABinding bnd) = superCast $ toNative bnd

-- instance NativeConverter Arguments where
--     type NativeType Arguments = List (NativeType Argument)
--     fromNative = map fromNative . fromJava
--     toNative = toJava . map toNative

-- instance NativeConverter FnReturn where
--     type NativeType FnReturn = Object
--     fromNative ret =
--         case safeDowncast ret of
--             Just sym -> Direct $ fromNative sym
--             Nothing -> Destructure $ map fromNative (fromJava $ (unsafeCast ret :: Collection Symbol))
--     toNative (Direct bnd) = superCast $ toNative bnd
--     toNative (Destructure l) = superCast $ (toJava $ map toNative l :: List Symbol)

instance NativeConverter FnId where
    type NativeType FnId = JInteger
    fromNative = FnId . fromJava
    toNative = toJava . unIRFnId

instance NativeConverter HostExpr where
    type NativeType HostExpr = JInteger
    fromNative = HostExpr . fromJava
    toNative = toJava . unwrapHostExpr

-- instance NativeConverter FnName where
--     type NativeType FnName = Symbol
--     fromNative sym = maybe (error "Symbol for fn name must be fully qualified") (`FnName` symGetName sym) (symGetNamespace sym)
--     toNative n = createSymbol (n^.namespace) (n^.name)

-- instance NativeConverter Binding where
--     type NativeType Binding = Symbol
--     toNative = createSymbol1 . unBinding
--     fromNative sym = maybe (Binding $ symGetName sym) (const $ error "Symbol for binding must not be qualified") (symGetNamespace sym)

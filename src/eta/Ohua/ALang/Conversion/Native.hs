-- |
-- Module      : $Header$
-- Description : Conversion from the Java native mirror language to the algorithm language
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.ALang.Conversion.Native where

import           Java
import           Ohua.ALang.Lang
import           Ohua.Compat
import           Ohua.Types

bindingFromNative :: NBinding -> Binding
bindingFromNative nbind = Binding (fromJava $ nBindingValue nbind)

assignmentFromNative :: NAssignment -> Assignment
assignmentFromNative assign =
    case (safeDowncast assign, safeDowncast assign) of
        (Just dir, _) -> Direct $ bindingFromNative $ nDirectAssignmentBinding dir
        (_, Just destr) -> Destructure $ map bindingFromNative $ fromJava (destr :: NBindingArr)
        _ -> error "unconvertable"

fnNameFromNative :: NFnName -> FnName
fnNameFromNative fnName = FnName (fromJava $ nFnNameName fnName) (fromJava $ nFnNameNamespace fnName)

resolvedBindingFromNative :: NResolvedSymbol -> ResolvedSymbol
resolvedBindingFromNative nbind =
    case (safeDowncast nbind, safeDowncast nbind, safeDowncast nbind, safeDowncast nbind) of
        (Just v, _, _, _) -> Local $ bindingFromNative $ nLocalBindingBinding v
        (_, Just v, _, _) -> Sf (fnNameFromNative $ nSfBindingFnName v) (fmap (FnId . fromJava) $ nSfBindingId v)
        (_, _, Just v, _) -> Algo (fnNameFromNative $ nAlgoBindingAlgoName v)
        (_, _, _, Just v) -> Env $ HostExpr $ fromJava $ nEnvBindingId v
        _ -> error "unconvertable"


exprFromNative :: NExpr -> Expression
exprFromNative nexpr =
    case (safeDowncast nexpr, safeDowncast nexpr, safeDowncast nexpr, safeDowncast nexpr) of
        (Just v, _, _, _) ->
            Var (resolvedBindingFromNative $ nVarExprValue v)
        (_, Just lam, _, _) ->
            Lambda (assignmentFromNative $ nLambdaExprAssignment lam) (exprFromNative $ nLambdaExprBody lam)
        (_, _, Just app, _) ->
            Apply (exprFromNative $ nApplyExprFunction app) (exprFromNative $ nApplyExprArgument app)
        (_, _, _, Just let_) ->
            Let (assignmentFromNative $ nLetExprAssignment let_)
                (exprFromNative $ nLetExprValue let_)
                (exprFromNative $ nLetExprBody let_)
        _ -> error "unconvertable"


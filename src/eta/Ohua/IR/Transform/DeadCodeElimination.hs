{-|
Module      : $Header$
Description : IR transformation to eliminate functions whose result is not used.
Copyright   : (c) Sebastian Ertel 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : POSIX

This source code is licensed under the terms described in the associated LICENSE.TXT file.
-}
{-# LANGUAGE MagicHash #-}
module Ohua.IR.Transform.DeadCodeElimination (transform) where

import Control.Monad.State
import Control.Monad.Extra
import Lens.Micro
import Lens.Micro.Mtl
import Java
import Ohua.IR
import Ohua.IR.Transform
import Ohua.Compat
import Ohua.IR.Functions
import Control.Monad.Loops
import Ohua.LensClasses
import Ohua.Types

import qualified Data.Set as S

type DeadCodeTransformer = Transformer CtxtMap

builtInOhuaFns :: S.Set FnName
builtInOhuaFns = S.fromList [oneToNName, collectName, idName, smapName, smapIOName, sizeName, seqName, algoOutName, selectName]

hasNoConsumers :: IRFn -> DeadCodeTransformer Bool
hasNoConsumers irFn = 
    null . concat <$> mapM getConsumers (extractBindings (irFn^.returnField))

isBuiltInOhuaFn :: IRFn -> Bool
isBuiltInOhuaFn irFn = S.member (irFn^.name) builtInOhuaFns

findCandidate :: DeadCodeTransformer (Maybe IRFn)
findCandidate = findM (\irFn -> fmap (&& isBuiltInOhuaFn irFn) (hasNoConsumers irFn)) =<< use graph
--     candidate <- findM (\irFn ->
--         do
--             let ib = isBuiltInOhuaFn irFn
--             nc <- hasNoConsumers irFn
--             return (ib && nc))
--         currentGraph

transform :: DeadCodeTransformer ()
transform = whileJust_ findCandidate deleteNode

executeFromNative :: NativeType IRGraph -> NativeType CtxtMap -> Java DeadCodeElimination (Map Keyword Object)
executeFromNative = executeTransformFromNativeWithCtxtMapWithoutInput transform

data {-# CLASS "com.ohua.ir.transform.DeadCodeElimination" #-} DeadCodeElimination = DeadCodeElimination (Object# DeadCodeElimination)

foreign export java "execute" executeFromNative :: NativeType IRGraph -> NativeType CtxtMap -> Java DeadCodeElimination (Map Keyword Object)


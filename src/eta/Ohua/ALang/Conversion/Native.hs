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


outGraphToNative :: OutGraph -> NativeGraph
outGraphToNative (OutGraph ops arcs) = newNGraph ops arcs
  where
    ops = toJava $ map newNOperator

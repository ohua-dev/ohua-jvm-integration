{-|
Module      : $Header$
Description : IR transformation to facilitate a parallel map.
Copyright   : (c) Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

This source code is licensed under the terms described in the associated LICENSE.TXT file.
-}
{-# LANGUAGE MagicHash, BangPatterns #-}
module Ohua.IR.Transform.Pmap 
    (transform) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import Control.Arrow
import Data.Maybe
import Lens.Micro
import Lens.Micro.Mtl
import Java
import Data.Function (on)
import Data.Hashable
import Control.Exception
import Debug.Trace
import Data.IORef
import System.IO
import Ohua.IR
import Ohua.IR.Transform
import Ohua.Compat
import Ohua.IR.Functions
import Ohua.LensClasses
import Ohua.ALang.Lang
import Unsafe.Coerce

type PmapTransformer = Transformer CtxtMap


getContext :: ToIRFnId a => a -> PmapTransformer CtxtStack
getContext a = fromJust . HM.lookup (toIRFnId a) <$> use other

foreign import java "@static com.ohua.util.Eta.widthToEnvArg" _widthAsEnvArg :: Int -> Object

widthAsEnvArg :: PmapWidth -> Argument
widthAsEnvArg = AEnv . unsafeCoerce . _widthAsEnvArg . unPmapWidth


transformSmap :: PmapWidth -> IRFnId -> PmapTransformer Argument
transformSmap batchSize target = do
    Just smap <- findFunc target
    let isSmapIO
            | smap^.name == smapName = False 
            | smap^.name == smapIOName = True
            | otherwise = error ("unrecognized context type " ++ show (smap^.name))
    
    collectNode <- if isSmapIO
        then do
            let Destructure (_:bnd:_) = smap^.returnField
            [otnFn] <- filter ((== oneToNName) . (^.name)) <$> getConsumers bnd
            let Direct otnRet = otnFn^.returnField
            [coll] <- filter ((== collectName) . (^.name)) <$> getConsumers otnRet
            return coll
        else do
            let (ABinding bnd:_) = smap^.arguments
            otnNode <- getProducer bnd
            let (ABinding obnd:_) = otnNode^.arguments
            [collectOtn] <- filter (\n -> n /= otnNode && n^.name == oneToNName) <$> getConsumers obnd
            let Direct obnd = collectOtn^.returnField
            [coll] <- filter ((== collectName) . (^.name)) <$> getConsumers obnd
            return coll
    (if isSmapIO
        then wrapSmapIO
        else wrapSmap) batchSize smap collectNode


wrapSmap, wrapSmapIO :: PmapWidth -> IRFn -> IRFn -> PmapTransformer Argument

wrapSmap batchSize smapNode collectNode = do
    let ABinding bnd = smapNode^?!arguments.ix 0
    otnNode <- getProducer bnd
    let [ABinding oldSizeOut, oldMapIn] = otnNode^.arguments
    oldSizeOp <- getProducer oldSizeOut

    (chunkFunc, chunkOut) <- newFnAndReturn chunkName [widthAsEnvArg batchSize, oldMapIn]
    
    outerSmapReturn <- newBinding

    (innerSize, innerSizeReturn) <- newFnAndReturn sizeName [ABinding outerSmapReturn]

    (innerOtn, innerOtnOut) <- newFnAndReturn oneToNName [ABinding innerSizeReturn, ABinding outerSmapReturn]

    innerSmap <- newFn smapName [ABinding innerOtnOut] (smapNode^.returnField)

    (innerSize1N, innerSize1NReturn) <- newFnAndReturn oneToNName [ABinding innerSizeReturn, ABinding innerSizeReturn]

    (innerCollect, innerCollectReturn) <- newFnAndReturn collectName [ABinding innerSize1NReturn, (collectNode^?!arguments.ix 1)]

    (outerSize1N, outerSize1NReturn) <- newFnAndReturn oneToNName [ABinding oldSizeOut, ABinding oldSizeOut]

    outerCollectReturn <- newBinding

    flattener <- newFn flattenerName [ABinding outerCollectReturn] (collectNode^.returnField)

    updateGraph $ HM.fromList $ map (first toIRFnId)
        [ (otnNode, [otnNode & arguments.ix 1 .~ ABinding chunkOut])
        , (oldSizeOp, [oldSizeOp & arguments .~ [ABinding chunkOut]])
        , (smapNode, [ chunkFunc
                     , smapNode & returnField .~ Destructure [outerSmapReturn]
                     , innerSize
                     , innerOtn
                     , innerSmap
                     , innerSize1N
                     , outerSize1N
                     ])
        , (collectNode, [ innerCollect
                        , collectNode 
                            & arguments .~ [ABinding outerSize1NReturn, ABinding innerCollectReturn]
                            & returnField .~ Direct outerCollectReturn
                        , flattener
                        ])
        ]
    return $ ABinding innerSizeReturn


foreign import java "@static com.ohua.util.Eta.wrapCollSource" wrapCollSource :: Int -> Object -> Object


wrapSmapIO batchSize smapNode collectNode = do
    outerSmapReturn <- newBinding

    (innerSize, innerSizeOut) <- newFnAndReturn sizeName [ABinding outerSmapReturn]

    (innerOtn, innerOtnOut) <- newFnAndReturn oneToNName [ABinding innerSizeOut, ABinding outerSmapReturn]

    innerSmap <- newFn smapName [ABinding innerOtnOut] (Destructure [smapNode^?!returnField._Destructure.ix 0])

    (innerSizeOtn, innerSizeOtnReturn) <- newFnAndReturn oneToNName [ABinding innerSizeOut, ABinding innerSizeOut]

    (innerCollect, innerCollectOut) <- newFnAndReturn collectName [ABinding innerSizeOtnReturn, collectNode^?!arguments.ix 1]

    (outerSizeOtn, outerSizeOtnReturn) <- newFnAndReturn oneToNName [ABinding oldSizeOut, ABinding oldSizeOut]

    outerCollectOut <- newBinding

    flattener <- newFn flattenerName [ABinding outerCollectOut] (collectNode^.returnField)

    updateGraph $ HM.fromList $ map (first toIRFnId) $
        [ (smapNode, [ smapNode & returnField._Destructure.ix 0 .~ outerSmapReturn
                                & arguments .~ [AEnv $ unsafeCoerce lazyCollSource]
                     , innerSize
                     , innerOtn
                     , innerSmap
                     , innerSizeOtn
                     , outerSizeOtn
                     ])
        , (collectNode, [innerCollect
                        , collectNode & arguments .~ [ABinding outerSizeOtnReturn, ABinding innerCollectOut]
                                      & returnField .~ Direct outerCollectOut
                        ])
        ]
    return $ ABinding innerSizeOut

  where
    oldSizeOut = smapNode^?!returnField._Destructure.ix 1
    collSource0 = unsafeCoerce $ smapNode^?!arguments.ix 0._AEnv :: Object
    collSource = unsafeCast $ collSource0 :: Collection Object

    lazyCollSource = wrapCollSource (unPmapWidth batchSize) (fromJava collSource !! 2)
    -- TODO implement. Should be:
    -- `(fn [] ^{:java-type LazyChunkedIterableWrapper} (new LazyChunkedIterableWrapper ~bss-value ~coll-source))




transformOne :: IRFnId -> Argument -> PmapWidth -> PmapTransformer ()
transformOne target sizeSource batchWidth = do
    Just targetNode <- findFunc target
    (packager, packagerReturn) <- newFnAndReturn packagerName (targetNode^.arguments)
    (size1N, size1NReturn) <- newFnAndReturn oneToNName [sizeSource, sizeSource]
    (collect, collectReturn) <- newFnAndReturn collectName [ABinding size1NReturn, ABinding packagerReturn]
    otherFns <- replicateM (pred (unPmapWidth batchWidth)) $ 
        case targetNode^.arguments of
            [] -> error "pmap for no-args functions is not supported"
            [_] -> do
                fnIn <- newBinding
                (fn, fnOut) <- newFnAndReturn (targetNode^.name) [ABinding fnIn]
                return (fnIn, fnOut, [fn])
            _ -> do
                pmapOut <- newBinding
                fnIn <- newBindings (length $ targetNode^.arguments)
                idFunc <- newFn idName [ABinding pmapOut] (Destructure fnIn)
                (fn, fnOut) <- newFnAndReturn (targetNode^.name) (map ABinding fnIn)
                return (pmapOut, fnOut, [idFunc, fn])
    newFnAndId <- 
        case targetNode^.arguments of
            [] -> error "pmap for no-args functions is not supported"
            [_] -> do
                fnIn <- newBinding
                fnOut <- newBinding
                let newFunc = targetNode & returnField .~ Direct fnOut 
                                         & arguments .~ [ABinding fnIn]
                return (fnIn, fnOut, [newFunc])
            _ -> do
                pmapOut <- newBinding
                fnOut <- newBinding
                fnIn <- newBindings (length $ targetNode^.arguments)
                idFunc <- newFn idName [ABinding pmapOut] (Destructure fnIn)
                let newFunc = targetNode & arguments .~ map ABinding fnIn
                                         & returnField .~ Direct fnOut
                return (pmapOut, fnOut, [idFunc, newFunc])
    let (allArgs, allReturns, allNodes) = unzip3 $ newFnAndId : otherFns
    pmapFn <- newFn pmapName [widthAsEnvArg batchWidth, ABinding collectReturn] (Destructure allArgs)
    (pCollect, pCollectReturn) <- newFnAndReturn pCollectName (widthAsEnvArg batchWidth:map ABinding allReturns)

    (otn, otnReturn) <- newFnAndReturn oneToNName [sizeSource, ABinding pCollectReturn]

    smap <- do
        (smapOut, idFn) <- 
            case targetNode^.returnField of
                Direct ret -> return (ret, [])
                dest -> do
                    smapOut <- newBinding
                    idFn <- newFn idName [ABinding smapOut] dest
                    return (smapOut, [idFn])
        
        (:idFn) <$> newFn smapName [ABinding otnReturn] (Destructure [smapOut])
    
    let all = packager:size1N:collect:pmapFn:join allNodes ++ (pCollect:otn:smap)

    replaceNode targetNode all


newtype PmapWidth = PmapWidth { unPmapWidth :: Int }

instance NativeConverter PmapWidth where
    type NativeType PmapWidth = JInteger
    fromNative = PmapWidth . fromJava
    toNative = toJava . unPmapWidth

-- TODO this needs to change. The list should be id+pmapWidth
newtype PmapInput = PmapInput [(IRFnId, PmapWidth)]

instance NativeConverter PmapInput where
    type NativeType PmapInput = Collection (Collection JInteger)
    fromNative nativeColl = PmapInput idList
      where
        idList = 
            [ (fromNative nativeId, fromNative nativeWidth)
            | coll <- fromJava nativeColl :: [Collection JInteger]
            , let [nativeId, nativeWidth] = fromJava coll
            ]

    toNative = error "is it this?" -- No implementation needed as this is never called for this type (for now)


transform :: PmapInput -> PmapTransformer ()
transform (PmapInput targetTuples) = do
    associatedTargets <- HM.fromList . zip targets . map (^.sourceFn) <$> mapM getTopmostCtxFrame targets
    let taintedSmaps = HM.elems associatedTargets
    sizeReturns <- mapM (uncurry transformSmap) $ zip sizeSources taintedSmaps
    let sizeMap = HM.fromList $ zip taintedSmaps sizeReturns
    forM_ targetTuples $ \(target, batchSizeSource) ->
        let Just sizeSource = HM.lookup (fromJust $ HM.lookup target associatedTargets) sizeMap
        in transformOne target sizeSource batchSizeSource
  where
    (targets, sizeSources) = unzip targetTuples
    getTopmostCtxFrame = fmap head . getContext


executePmapFromNative :: NativeType IRGraph
                        -> NativeType CtxtMap
                        -> NativeType PmapInput
                        -> Java Pmap (Map Keyword Object)
executePmapFromNative = executeTransformFromNativeWithCtxtMap transform


data {-# CLASS "com.ohua.ir.transform.Pmap" #-} Pmap = Pmap (Object# Pmap)

foreign export java "execute" executePmapFromNative :: NativeType IRGraph -> NativeType CtxtMap -> NativeType PmapInput -> Java Pmap (Map Keyword Object)


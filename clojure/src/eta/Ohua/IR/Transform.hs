{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ohua.IR.Transform where

import Ohua.IR
import Ohua.Compat
import Lens.Micro
import Lens.Micro.Mtl
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.State
import Data.List (find)
import Java
import qualified Data.HashMap.Strict as HM
import Ohua.LensClasses
import Ohua.Types
import Ohua.ALang.Lang

type BindingSupplier = [Binding]
type IdSupplier = [IRFnId]

data TransformationState a = TransformationState
    { transformationStateGraph :: !IRGraph
    , transformationStateBindingSupplier :: !BindingSupplier
    , transformationStateIdSupplier :: !IdSupplier
    , transformationStateOther :: !a
    }

instance HasGraph (TransformationState a) IRGraph where graph = lens transformationStateGraph (\s a -> s {transformationStateGraph=a})
instance HasBindingSupplier (TransformationState a) BindingSupplier where bindingSupplier = lens transformationStateBindingSupplier (\s a -> s {transformationStateBindingSupplier=a})
instance HasIdSupplier (TransformationState a) IdSupplier where idSupplier = lens transformationStateIdSupplier (\s a -> s {transformationStateIdSupplier=a})
instance HasOther (TransformationState a) a where other = lens transformationStateOther (\s a -> s {transformationStateOther=a})

newtype Transformer a r = Transformer (StateT (TransformationState a) IO r) deriving (Functor, Applicative, Monad, MonadIO, MonadState (TransformationState a))


findFunc :: ToIRFnId a => a -> Transformer t (Maybe IRFn)
findFunc target = do
    g <- use graph
    return $ find ((== toIRFnId target) . toIRFnId) g

getProducer :: Binding -> Transformer a IRFn
getProducer bnd = fromJust . find (returns bnd) <$> use graph

getConsumers :: Binding -> Transformer a [IRFn]
getConsumers bnd = filter (consumes bnd) <$> use graph

newBinding :: Transformer t Binding
newBinding = do
    (h:t) <- use bindingSupplier
    bindingSupplier .= t
    return h


newBindings :: Int -> Transformer t [Binding]
newBindings i = do
    (h,t) <- splitAt i <$> use bindingSupplier
    bindingSupplier .= t
    return h


newIRFnId :: Transformer t IRFnId
newIRFnId = do
    (h:t) <- use idSupplier
    idSupplier .= t
    return h


newFn :: FnName -> Arguments -> FnReturn -> Transformer a IRFn
newFn name args ret = do
    id <- newIRFnId
    return $ IRFn id name args ret


newFnAndReturn :: FnName -> Arguments -> Transformer a (IRFn, Binding)
newFnAndReturn name args = do
    retBinding <- newBinding
    fn <- newFn name args (Direct retBinding)
    return (fn, retBinding)


modifyGraph :: (IRGraph -> IRGraph) -> Transformer a ()
modifyGraph = (graph %=)


replaceNode :: ToIRFnId a => a -> [IRFn] -> Transformer b ()
replaceNode n funcs = modifyGraph (\l -> let (h, _:t) = break ((== toIRFnId n) . toIRFnId) l in h ++ funcs ++ t)

deleteNode :: ToIRFnId a => a -> Transformer b ()
deleteNode n = replaceNode n []

updateGraph :: HM.HashMap IRFnId [IRFn] -> Transformer b ()
updateGraph replacementMap = modifyGraph $ concatMap $ \elem ->
    fromMaybe [elem] $ HM.lookup (elem^.idField) replacementMap


bindingSupplierFromGraph :: IRGraph -> BindingSupplier
bindingSupplierFromGraph graph = 
    [ bnd 
    | num <- Nothing : map Just [0..]
    , char <- ['a'..'z']
    , let bnd = Binding $ char:maybe "" show num
    , not $ HS.member bnd taken 
    ]
  where
    taken = HS.fromList $ extractBindings graph


idSupplierFromGraph :: IRGraph -> IdSupplier
idSupplierFromGraph graph = map FnId [largestId..]
  where
    largestId = succ $ maximum $ map (unIRFnId . (^.idField)) graph


runTransformer :: IRGraph -> a -> Transformer a b -> IO (IRGraph, a, b)
runTransformer inGraph a (Transformer t) = do
    (b, state) <- runStateT t $ 
        TransformationState
            inGraph
            bindingSupplier
            idSupplier
            a
    return (state^.graph, state^.other, b)
  where 
    bindingSupplier = bindingSupplierFromGraph inGraph
    idSupplier = idSupplierFromGraph inGraph

executeTransformFromNativeWithCtxtMap :: NativeConverter input
                                      => (input -> Transformer CtxtMap ())
                                      -> NativeType IRGraph
                                      -> NativeType CtxtMap
                                      -> NativeType input
                                      -> Java a (Map Keyword Object) -- this array contains [graph, ctxtMap]
executeTransformFromNativeWithCtxtMap ft ngraph nmap nativeInput = do
    (graph, ctxtMap, _) <- io $ runTransformer (fromNative ngraph) (fromNative nmap) (ft $ fromNative nativeInput)
    return $ toJava
        [ (graphKW, superCast $ toNative graph :: Object)
        , (ctxtMapKW, superCast $ toNative ctxtMap)
        ]
  where
    graphKW = createKeyword "graph"
    ctxtMapKW = createKeyword "ctxt-map"

executeTransformFromNativeWithCtxtMapWithoutInput :: Transformer CtxtMap ()
                                                  -> NativeType IRGraph
                                                  -> NativeType CtxtMap
                                                  -> Java a (Map Keyword Object) -- this array contains [graph, ctxtMap]
executeTransformFromNativeWithCtxtMapWithoutInput ft ngraph nmap = do
    (graph, ctxtMap, _) <- io $ runTransformer (fromNative ngraph) (fromNative nmap) ft
    return $ toJava
        [ (graphKW, superCast $ toNative graph :: Object)
        , (ctxtMapKW, superCast $ toNative ctxtMap)
        ]
  where
    graphKW = createKeyword "graph"
    ctxtMapKW = createKeyword "ctxt-map"

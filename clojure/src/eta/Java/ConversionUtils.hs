{-# LANGUAGE BangPatterns #-}
module Java.ConversionUtils where

import Java
import qualified Ohua.Util.Str as Str


instance Show Object where 
    show = maybe "null" (fromJString . toString) . (maybeFromJava :: Object -> Maybe Object)

class Class (NativeType a) => NativeConverter a where
    type NativeType a
    fromNative :: NativeType a -> a
    toNative :: a -> NativeType a

instance NativeConverter String where
    type NativeType String = JString
    fromNative = fromJava
    toNative = toJava

instance NativeConverter Str.Str where
    type NativeType Str.Str = JString
    fromNative = Str.fromString . fromJava
    toNative = toJava . Str.toString

asBool :: Object -> Bool
asBool !o = (fromJava :: JBoolean -> Bool) . unsafeCast $ o

asString :: Object -> Str.Str
asString = (fromNative :: JString -> Str.Str) . unsafeCast

instance JavaConverter a a where
    toJava = id
    fromJava = id
    
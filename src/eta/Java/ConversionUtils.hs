{-# LANGUAGE BangPatterns #-}
module Java.ConversionUtils where

import Java
import qualified Data.Text as T

asBool :: Object -> Bool
asBool !o = (fromJava :: JBoolean -> Bool) . unsafeCast $ o

toText :: JString -> T.Text
toText = T.pack . fromJava

asString :: Object -> T.Text
asString = toText . unsafeCast

instance JavaConverter a a where
    toJava = id
    fromJava = id
    
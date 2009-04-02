{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Data.Binary.Star
    where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)

data WrapPut t = WrapPut { unWrap ::t -> Put }

type MyGet = Get
type MyPut = WrapPut

class Pickler d t where
    pickle :: d t

instance Binary a => Pickler Get a where
    pickle = get

instance Binary a => Pickler WrapPut a where
    pickle = WrapPut { unWrap = put }

myDecode :: Pickler Get a => ByteString -> a
myDecode =  runGet pickle

myEncode :: Pickler WrapPut a => a -> ByteString
myEncode = runPut . unWrap pickle

instance Serializable MyType where
    serialize = do

{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Serialize
Description : serialization and deserialization
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines classes for binary serialization
and deserialization.
-}

module Data.Auth.Serialize
    ( Serializable (..)
    , Deserializable (..)
    , GSerializable (..)
    , GDeserializable (..)
    , serialize
    , deserialize
    , unsafeDeserialize
    ) where

import qualified Data.Binary          as B
import           Data.Binary.Get      (Get, runGetOrFail)
import           Data.Binary.Put      (Put, runPut)
import           Data.ByteString.Lazy (ByteString)
import           Data.Word            (Word8)
import           GHC.Generics

-- | Classifies serializable types.
class Serializable a where

    put :: a -> Put

    default put :: (Generic a, GSerializable (Rep a)) => a -> Put
    put = gput . from

-- | Classifies deserializable types.
class Deserializable a where

    get :: Get a

    default get :: (Generic a, GDeserializable (Rep a)) => Get a
    get = to <$> gget

-- | Serializes the argument into a lazy 'ByteString'.
--
-- >>> serialize True
-- "\STX"
--
serialize :: Serializable a => a -> ByteString
serialize = runPut . put

-- | Tries to deserialize an @a@ from a lazy 'ByteString'.
-- If this fails, an error message is returned.
-- If it succeeds, a pair consisting of unconsumed input and deserialzed value
-- is returned.
--
-- >>> deserialize (serialize True) :: Either String (ByteString, Bool)
-- Right ("",True)
--
-- >>> deserialize (serialize (42 :: Int)) :: Either String (ByteString, Bool)
-- Left "deserialize: expected 1 or 2, but got 0"
--
deserialize :: Deserializable a => ByteString -> Either String (ByteString, a)
deserialize bs = case runGetOrFail get bs of
    Left  (_  , _, e) -> Left e
    Right (bs', _, a) -> Right (bs', a)

-- | Adding generics support for 'Serializable'.
class GSerializable f where
    gput :: f t -> Put

-- | Adding generics support for 'Deserializable'.
class GDeserializable f where
    gget :: Get (f t)

instance (GSerializable f, GSerializable g) => GSerializable (f :+: g) where
    gput (L1 x) = B.putWord8 1 >> gput x
    gput (R1 y) = B.putWord8 2 >> gput y

instance (GSerializable f, GSerializable g) => GSerializable (f :*: g) where
    gput (x :*: y) = gput x >> gput y

instance GSerializable U1 where
    gput U1 = return ()

instance GSerializable f => GSerializable (M1 i c f) where
    gput (M1 x) = gput x

instance Serializable c => GSerializable (K1 i c) where
    gput (K1 c) = put c

instance (GDeserializable f, GDeserializable g) => GDeserializable (f :+: g) where
    gget = do
        i <- B.getWord8
        case i of
            1 -> L1 <$> gget
            2 -> R1 <$> gget
            b -> fail $ "deserialize: expected 1 or 2, but got " ++ show b

instance (GDeserializable f, GDeserializable g) => GDeserializable (f :*: g) where
    gget = (:*:) <$> gget <*> gget

instance GDeserializable U1 where
    gget = return U1

instance GDeserializable f => GDeserializable (M1 i c f) where
    gget = M1 <$> gget

instance Deserializable c => GDeserializable (K1 i c) where
    gget = K1 <$> get

instance Serializable Int where
    put = B.put

instance Serializable Double where
    put = B.put

instance Serializable Char where
    put = B.put

instance Serializable Word8 where
    put = B.put

deriving instance Serializable Bool
deriving instance Serializable a => Serializable [a]
deriving instance (Serializable a, Serializable b) => Serializable (a, b)

instance Deserializable Int where
    get = B.get

instance Deserializable Double where
    get = B.get

instance Deserializable Char where
    get = B.get

instance Deserializable Word8 where
    get = B.get

deriving instance Deserializable Bool
deriving instance Deserializable a => Deserializable [a]
deriving instance (Deserializable a, Deserializable b) => Deserializable (a, b)

-- | Unsafe version of 'deserialize' which can fail and ignores unconsumed
-- input.
--
-- >>> unsafeDeserialize (serialize False) :: Bool
-- False
--
-- >>> unsafeDeserialize (serialize False) :: Int
-- *** Exception: not enough bytes
-- ...
--
unsafeDeserialize :: Deserializable a => ByteString -> a
unsafeDeserialize bs = case deserialize bs of
    Left e       -> error e
    Right (_, x) -> x

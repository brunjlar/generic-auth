{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Hash
Description : hashing
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides an implementation of cryptographic hashing.
-}

module Data.Auth.Hash
    ( Hash
    , hash
    ) where

import qualified Crypto.Hash            as C
import           Data.Binary            (Binary)
import qualified Data.Binary            as B
import qualified Data.ByteArray         as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B8
import           Data.ByteString.Lazy   (toStrict)
import           Test.QuickCheck

import           Data.Auth.Serialize

-- | A /hash/, implemented as a SHA256 digest.
newtype Hash = Hash {getHash :: C.Digest C.SHA256}
    deriving (Eq, Ord)

instance Show Hash where

    show = show . getHash

instance Read Hash where

    readsPrec _ s
        | length s < 64 = []
        | otherwise     =
            let (s', s'') = splitAt 64 s
            in  if all (`elem` "0123456789abcdef") s'
                    then
                        let (bs, _) = B16.decode $ B8.pack s'
                            Just d  = C.digestFromByteString bs
                        in  [(Hash d, s'')]
                    else []

instance Binary Hash where

    get = do
        md <- C.digestFromByteString . B.pack <$> get
        case md of
            Nothing -> fail "expected hash"
            Just d  -> return $ Hash d

    put = put . A.unpack . getHash

instance Serializable Hash where
    put = B.put

instance Deserializable Hash where
    get = B.get

instance Arbitrary Hash where

    arbitrary = do
        n <- arbitrary :: Gen Int
        return $ hash n

    shrink = const [] -- Shrinking a hash makes no sense.

-- | Hashes a serializable value using the SHA256 cryptographic hashing
-- algorithm. This is the only way to generate a value of type @'Hash'@,
-- because the constructor of @'Hash'@ is not exported.
--
-- >>> hash "xyz"
-- d5554fb30e572f7a125e1b42223fd77c866a9773a3435b9c268dbe5dc3175957
--
hash :: Serializable a => a -> Hash
hash = Hash . C.hash . toStrict . serialize

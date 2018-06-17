{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Utils.Hash
Description : hashing
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides an implementation of cryptographic hashing.
-}

module Data.Auth.Util.Hash
    ( Hash
    , hash
    ) where

import qualified Crypto.Hash            as C
import           Data.Binary            (Binary (..), encode)
import qualified Data.ByteArray         as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B8
import           Data.ByteString.Lazy   (toStrict)

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
        md <- (C.digestFromByteString . B.pack) <$> get
        case md of
            Nothing -> fail "expected hash"
            Just d  -> return $ Hash d

    put = put . A.unpack . getHash

-- | Hashes a serializable value using the SHA256 cryptographic hashing
-- algorithm.
--
-- >>> hash "xyz"
-- da5e5e70038e631c22d902e912e605cff9dd71a0180bd4c2681447e6bd7fca7c
--
hash :: Binary a => a -> Hash
hash = Hash . C.hash . toStrict . encode

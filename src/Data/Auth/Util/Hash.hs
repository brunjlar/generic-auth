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

import qualified Crypto.Hash          as C
import           Data.Binary          (Binary (..), encode)
import qualified Data.ByteArray       as A
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (toStrict)

-- | A /hash/, implemented as a SHA256 digest.
newtype Hash = Hash {getHash :: C.Digest C.SHA256}
    deriving Eq

instance Show Hash where

    show = show . getHash

instance Binary Hash where

    get = do
        md <- (C.digestFromByteString . B.pack) <$> get
        case md of
            Nothing -> fail "expected hash"
            Just d  -> return $ Hash d

    put = put . A.unpack . getHash

-- | Hashes a serializable value using the SHA256 cryptographic hashing
-- algorithm.
hash :: Binary a => a -> Hash
hash = Hash . C.hash . toStrict . encode

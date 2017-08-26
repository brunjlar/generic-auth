module Data.Auth.Util.Hash
    ( Hash
    , hash
    ) where

import qualified Crypto.Hash          as C
import           Data.Binary          (Binary (..), encode)
import qualified Data.ByteArray       as A
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString      as B

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


hash :: Binary a => a -> Hash
hash = Hash . C.hash . toStrict . encode

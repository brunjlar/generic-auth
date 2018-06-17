module Data.Auth.Util.HashSpec
    ( spec
    ) where

import Data.Auth.Util.Hash
import Data.Binary         (decodeOrFail, encode)
import Test.Hspec
import Test.QuickCheck
import Text.Read           (readMaybe)

spec :: Spec
spec = describe "Hash" $ do
    it "has compatible Read- and Show-instances" $ property propHashReadShow
    it "has a sound Binary instance" $ property propHashBinary

propHashReadShow :: Hash -> Bool
propHashReadShow h = case readMaybe (show h) of
    Nothing -> False
    Just h' -> h == h'

propHashBinary :: Hash -> Bool
propHashBinary h = case decodeOrFail (encode h) of
    Left _           -> False
    Right (_, _, h') -> h == h'

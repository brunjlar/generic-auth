module Data.Auth.Examples.TreeSpec
    ( spec
    ) where

import Control.Monad           (forM_)
import Data.Auth.Core
import Data.Auth.Examples.Tree
import Test.Hspec

spec :: Spec
spec = describe "lookupTree" $ do
    let (t, _) = runProver exampleTree
        h      = toHash t
    forM_ paths $ \p ->
        it ("can be verified for path " ++ show p) $ do
            let (x, bs) = runProver $ lookupTree p t
            runVerifier' (lookupTree p) h bs `shouldBe` Right x

paths :: [[Direction]]
paths = do
    x <- [L, R]
    y <- [L, R]
    z <- [L, R]
    return [x, y, z]

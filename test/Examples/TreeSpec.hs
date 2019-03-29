{-# LANGUAGE DataKinds #-}

module Examples.TreeSpec
    ( spec
    ) where

import Control.Monad           (forM_)
import Test.Hspec

import Data.Auth
import Examples.Tree

spec :: Spec
spec = describe "lookupTree" $
    forM_ paths $ \p ->
        it ("can be verified for path " ++ show p) $ do
            let (x, bs) = runProver $ lookupTree p exampleTreeP
            runVerifier (lookupTree p exampleTreeV) bs `shouldBe` Right x

paths :: [[Direction]]
paths = do
    x <- [L, R]
    y <- [L, R]
    z <- [L, R]
    return [x, y, z]

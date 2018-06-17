module Data.Auth.Examples.TreeSpec
    ( spec
    ) where

import Data.Auth.Core
import Data.Auth.Examples.Tree
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Tree" $ return ()

authWorks :: Eq a => AuthM a -> Bool
authWorks p =
    let (a, bs) = runProver p
    in  case runVerifier p bs of
            Left _   -> False
            Right a' -> a == a'

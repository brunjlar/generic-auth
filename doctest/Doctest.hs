module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest [ "src/Data/Auth/Util/Hash.hs" ]
    doctest [ "src/Data/Auth/Internal/Monad.hs"
            , "src/Data/Auth/Examples/Tree.hs"
            ]

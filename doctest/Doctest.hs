module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest [ "src/Data/Auth/Hash.hs" ]
    doctest [ "src/Data/Auth/Monad.hs"
            , "src/Data/Auth/Examples/AdHocTree.hs"
            , "src/Data/Auth/Examples/List.hs"
            , "src/Data/Auth/Examples/Tree.hs"
            ]

module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest [ "src/Data/Auth/Serialize.hs"
            , "src/Data/Auth/Hash.hs"
            ]
    doctest [ "src/Data/Auth/Monad.hs"
            , "src/Examples/AdHocTree.hs"
            , "src/Examples/List.hs"
            , "src/Examples/Tree.hs"
            ]

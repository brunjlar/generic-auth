module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "src/Data/Auth/Examples/Tree.hs"
               , "src/Data/Auth/Internal/Monad.hs"
               , "src/Data/Auth/Util/Hash.hs"
               ]

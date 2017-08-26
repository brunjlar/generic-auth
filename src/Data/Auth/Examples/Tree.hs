module Data.Auth.Examples.Tree
    ( Tree (..)
    , example
    , lookupTree
    ) where

import Data.Auth.Core
import Data.Binary    (Binary)
import GHC.Generics

data Tree a =
      Tip a
    | Node (Auth (Tree a)) (Auth (Tree a))
    deriving (Show, Generic)

instance Binary a => Binary (Tree a) where

instance Authenticatable a => Authenticatable (Tree a) where

    shallowCopy (Tip a)    = Tip $ shallowCopy a
    shallowCopy (Node l r) = Node (shallowCopy l) (shallowCopy r)

example :: Tree String
example = Node
    (authP $ Node (authP $ Tip "Alice") (authP $ Tip "Bob"))
    (authP $ Node (authP $ Tip "Charlie") (authP $ Tip "Doris"))

exampleP :: Auth (Tree String)
exampleP = authP example

exampleV :: Auth (Tree String)
exampleV = authV $ shallowCopy example

data Direction = L | R deriving (Show, Eq)

lookupTree :: Authenticatable a => Auth (Tree a) -> [Direction] -> AuthM (Maybe a)
lookupTree at xs = do
    t <- unauth at
    case (t, xs) of
        (Tip a   , [])     -> return $ Just a
        (Tip _   , _)      -> return Nothing
        (Node _ _, [])     -> return Nothing
        (Node l _, L : ys) -> lookupTree l ys
        (Node _ r, R : ys) -> lookupTree r ys

test :: Show a => PureProver a -> PureVerifier a -> IO ()
test p v = do
    let (a, bs) = runPureProver p
    print a
    print bs
    let ma' = runPureVerifier v bs
    print ma'

testP :: PureProver (Maybe String)
testP = prover $ lookupTree exampleP [R, L]

testV :: PureVerifier (Maybe String)
testV = verifier $ lookupTree exampleV [R, L]

module Data.Auth.Examples.Tree
    ( Tree (..)
    , lookupTree
    , buildTree
    , exampleTree
    , test
    , testP
    , testV
    ) where

import Control.Monad.State
import Data.Auth.Core
import Data.Binary         (Binary)
import GHC.Generics

data Tree a =
      Tip a
    | Node (Auth (Tree a)) (Auth (Tree a))
    deriving (Show, Generic)

instance Binary a => Binary (Tree a)
instance Authenticatable a => Authenticatable (Tree a)

data Direction = L | R deriving (Show, Eq)

buildTree :: forall a. Authenticatable a => Int -> [a] -> AuthM (Auth (Tree a))
buildTree = evalStateT . go
  where
    go :: Int -> StateT [a] AuthM (Auth (Tree a))
    go 0 = do
        (y : ys) <- get
        put ys
        lift $ auth $ Tip y
    go d = do
        let d' = d - 1
        node <- Node <$> go d' <*> go d'
        lift $ auth node

lookupTree :: Authenticatable a => Auth (Tree a) -> [Direction] -> AuthM (Maybe a)
lookupTree at xs = do
    t <- unauth at
    case (t, xs) of
        (Tip a   , [])     -> return $ Just a
        (Tip _   , _)      -> return Nothing
        (Node _ _, [])     -> return Nothing
        (Node l _, L : ys) -> lookupTree l ys
        (Node _ r, R : ys) -> lookupTree r ys

exampleTree :: AuthM (Auth (Tree String))
exampleTree = buildTree 2 ["Alice", "Bob", "Charlie", "Doris"]

test :: Show a => PureProver a -> PureVerifier a -> IO ()
test p v = do
    let (a, bs) = runPureProver p
    print a
    print bs
    let ma' = runPureVerifier v bs
    print ma'

testP :: PureProver (Maybe String)
testP = prover $ do
    t <- exampleTree
    lookupTree t [R, L]

testV :: PureVerifier (Maybe String)
testV = verifier $ do
    t <- exampleTree
    lookupTree t [R, L]

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Examples.Tree
Description : authenticated binary tree example
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides the example of authenticated binary trees.
-}

module Data.Auth.Examples.Tree
    ( Tree (..)
    , Direction (..)
    , lookupTree
    , buildTree
    , exampleTree
    , exampleProg
    ) where

import Control.Monad.State
import Data.Auth.Core
import Data.Binary         (Binary)
import GHC.Generics

-- | A simple authenticated binary tree type.
data Tree a =
      Tip a
    | Node (Auth (Tree a)) (Auth (Tree a))
    deriving (Show, Generic, Binary, Authenticatable)

-- | Describes a direction (@'L'@eft or @'R'@ight), so that
-- a list of directions gives a path from the root of a @'Tree'@
-- to a @'Tip'@.
data Direction = L | R deriving (Show, Read, Eq, Ord)

-- | Builds a full @'Tree'@ with specified depth and specified tips.
-- Using a negative depth or providing too few tips causes an error.
buildTree :: forall a. Authenticatable a => Int -> [a] -> AuthM (Auth (Tree a))
buildTree = evalStateT . go
  where
    go :: Int -> StateT [a] AuthM (Auth (Tree a))
    go 0 = do
        (y : ys) <- get
        put ys
        lift $ auth $ Tip y
    go d
        | d < 0     = error "negative depth"
        | otherwise = do
            let d' = d - 1
            node <- Node <$> go d' <*> go d'
            lift $ auth node

-- | Looks up the tip of a tree to which the given path from the root leads.
-- Returns @'Nothing'@ if the path does not lead to a tip.
--
-- >>> fst $ runProver $ buildTree 1 ["Alice", "Bob"] >>= \t -> lookupTree t [L]
-- Just "Alice"
lookupTree :: Authenticatable a => Auth (Tree a) -> [Direction] -> AuthM (Maybe a)
lookupTree at xs = do
    t <- unauth at
    case (t, xs) of
        (Tip a   , [])     -> return $ Just a
        (Tip _   , _)      -> return Nothing
        (Node _ _, [])     -> return Nothing
        (Node l _, L : ys) -> lookupTree l ys
        (Node _ r, R : ys) -> lookupTree r ys

-- | Computation to build a simple example @'Tree'@ of depth three with eight
-- tips.
exampleTree :: AuthM (Auth (Tree String))
exampleTree = buildTree 3 ["Alice", "Bob", "Charlie", "Doris", "Eric", "Fred", "Gina", "Heather"]

-- | An example computation that looks up the tip at the specified path in
-- @'exampleTree'@.
exampleProg :: [Direction] -> AuthM (Maybe String)
exampleProg xs = do
    t <- exampleTree
    lookupTree t xs

test :: Show a => AuthM a -> AuthM a -> IO ()
test p v = do
    let (a, bs) = runProver p
    print a
    print bs
    let ea = runVerifier v bs
    print ea

test' :: [Direction] -> [Direction] -> IO ()
test' xs ys = test (exampleProg xs) (exampleProg ys)

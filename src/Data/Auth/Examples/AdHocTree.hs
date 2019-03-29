{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Examples.AdHocTree
Description : ad hoc binary tree example
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides the example of ad hoc binary trees.
-}

module Data.Auth.Examples.AdHocTree where

import Control.Monad.State

import Data.Auth

-- | A simple binary tree type.
data Tree a =
      Tip a
    | Node (Tree a) (Tree a)
    deriving (Show, Generic, Serializable)

-- | Describes a direction (@'L'@eft or @'R'@ight), so that
-- a list of directions gives a path from the root of a @'Tree'@
-- to a @'Tip'@.
data Direction = L | R deriving (Show, Read, Eq, Ord)

-- | Builds a full @'Tree'@ with specified depth and specified tips.
-- Using a negative depth or providing too few tips causes an error.
buildTree :: forall a. Int -> [a] -> Tree a
buildTree = evalState . go
  where
    go :: Int -> State [a] (Tree a)
    go 0 = do
        xs <- get
        case xs of
            []       -> error "buildTree: not enough tips"
            (y : ys) -> put ys >> return (Tip y)
    go d
        | d < 0     = error "negative depth"
        | otherwise = let d' = d - 1 in Node <$> go d' <*> go d'

-- | Hashes a tree by using the hash of the value at tips and the hash
-- of the pair of hashes of the children for nodes.
hash' :: Serializable a => Tree a -> Hash
hash' (Tip a)    = hash a
hash' (Node l r) = hash (hash' l, hash' r)

-- | Looks up the tip of a tree to which the given path from the root leads.
-- Throws an error if the path does not lead to a tip.
--
-- >>> lookupTree [L] $ buildTree 1 ["Alice", "Bob"]
-- ("Alice",[(fabad396304c47fff5efff4cfbf554dd982b6639c21045b087376685bdbd87a2,57b6b1f0ef29c035a7693dc5ff42f9b01cfb56da4c0ccce7fe0ea4aafa613678)])
--
lookupTree :: Serializable a => [Direction] -> Tree a -> (a, [(Hash, Hash)])
lookupTree []       (Tip a)    = (a, [])
lookupTree (d : xs) (Node l r) =
    let (a, hs) = lookupTree xs $ case d of L -> l; R -> r
    in  (a, (hash' l, hash' r) : hs)
lookupTree _        _          = error "illegal path"

-- | Checks the result of @lookupTree@, given the hash of the tree.
--
-- >>> let t = buildTree 1 ["Alice", "Bob"]
-- >>> checkLookup [R] (hash' t) $ lookupTree [R] t
-- True
checkLookup :: Serializable a => [Direction] -> Hash -> (a, [(Hash, Hash)]) -> Bool
checkLookup []       h (a, [])          = h == hash a
checkLookup (d : xs) h (a, (l, r) : hs) =
    h == hash (l, r) && checkLookup xs (case d of L -> l; R -> r) (a, hs)
checkLookup _        _ _                = False

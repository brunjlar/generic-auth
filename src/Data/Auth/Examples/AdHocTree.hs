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
    deriving (Show, Generic, Binary)

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
        (y : ys) <- get
        put ys
        return $ Tip y
    go d
        | d < 0     = error "negative depth"
        | otherwise = let d' = d - 1 in Node <$> go d' <*> go d'

-- | Hashes a tree by using the hash of the value at tips and the hash
-- of the pair of hashes of the children for nodes.
hash' :: Binary a => Tree a -> Hash
hash' (Tip a)    = hash a
hash' (Node l r) = hash (hash' l, hash' r)

-- | Looks up the tip of a tree to which the given path from the root leads.
-- Throws an error if the path does not lead to a tip.
--
-- >>> lookupTree [L] $ buildTree 1 ["Alice", "Bob"]
-- ("Alice",[(269a4cb9ff8b0330c43c9271c42ef60ffd0ad24103fcfaf3284c7295c43af7e5,9e15ae465acb02add61af20819223c5ff6045ccd0ce65c53965c8b8a29162894)])
lookupTree :: Binary a => [Direction] -> Tree a -> (a, [(Hash, Hash)])
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
checkLookup :: Binary a => [Direction] -> Hash -> (a, [(Hash, Hash)]) -> Bool
checkLookup []       h (a, [])          = h == hash a
checkLookup (d : xs) h (a, (l, r) : hs) =
    h == hash (l, r) && checkLookup xs (case d of L -> l; R -> r) (a, hs)
checkLookup _        _ _                = False

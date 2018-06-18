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
-- >>> fst $ runProver $ buildTree 1 ["Alice", "Bob"] >>= lookupTree [L]
-- Just "Alice"
lookupTree :: Authenticatable a => [Direction] -> Auth (Tree a) -> AuthM (Maybe a)
lookupTree xs at = do
    t <- unauth at
    case (t, xs) of
        (Tip a   , [])     -> return $ Just a
        (Tip _   , _)      -> return Nothing
        (Node _ _, [])     -> return Nothing
        (Node l _, L : ys) -> lookupTree ys l
        (Node _ r, R : ys) -> lookupTree ys r

-- | Computation to build a simple example @'Tree'@ of depth three with eight
-- tips.
--
-- >>> let (t, _) = runProver exampleTree
-- >>> let h = toHash t
-- >>> h
-- a651ca4e022aa8633799d1e717f00b4343ae08131dee9d3aae431f497c7bd7c2
-- >>> let (x, bs) = runProver $ lookupTree [L, L, R] t
-- >>> x
-- Just "Bob"
-- >>> runVerifier' (lookupTree [L, L, R]) h bs :: Either AuthError (Maybe String)
-- Right (Just "Bob")
--
exampleTree :: AuthM (Auth (Tree String))
exampleTree = buildTree 3 ["Alice", "Bob", "Charlie", "Doris", "Eric", "Fred", "Gina", "Heather"]

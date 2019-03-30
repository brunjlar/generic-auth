{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Examples.Tree
Description : authenticated binary tree example
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides the example of authenticated binary trees.
-}

module Examples.Tree
    ( Tree (..)
    , Direction (..)
    , lookupTree
    , buildTree
    , exampleTree
    , exampleTreeP
    , exampleTreeV
    ) where

import Control.Monad.State
import Data.Auth

-- | A simple authenticated binary tree type.
data Tree a i =
      Tip a
    | Node (Auth (Tree a i) i) (Auth (Tree a i) i)
    deriving (Show, Generic)

deriving instance Serializable a => Serializable (Tree a 'P)
deriving instance Serializable a => Serializable (Tree a 'V)
deriving instance Deserializable a => Deserializable (Tree a 'P)
deriving instance Deserializable a => Deserializable (Tree a 'V)

-- | Describes a direction (@'L'@eft or @'R'@ight), so that
-- a list of directions gives a path from the root of a @'Tree'@
-- to a @'Tip'@.
data Direction = L | R deriving (Show, Read, Eq, Ord)

-- | Builds a full @'Tree'@ with specified depth and specified tips.
-- Using a negative depth or providing too few tips causes an error.
buildTree :: forall i a. Serializable (Tree a i)
          => Int
          -> [a]
          -> AuthM i (Auth (Tree a i) i)
buildTree = evalStateT . go
  where
    go :: Int -> StateT [a] (AuthM i) (Auth (Tree a i) i)
    go 0 = do
        xs <- get
        case xs of
            []       -> error "buildTree: not enough tips"
            (y : ys) -> put ys >> lift (auth $ Tip y)
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
lookupTree :: (Serializable (Tree a i), Deserializable (Tree a i))
           => [Direction]
           -> Auth (Tree a i) i
           -> AuthM i (Maybe a)
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
-- >>> let (x, bs) = runProver $ lookupTree [L, L, R] exampleTreeP
-- >>> x
-- Just "Bob"
-- >>> runVerifier (lookupTree [L, L, R] exampleTreeV) bs
-- Right (Just "Bob")
--
exampleTree :: Serializable (Tree String i) => AuthM i (Auth (Tree String i) i)
exampleTree = buildTree 3 [ "Alice"
                          , "Bob"
                          , "Charlie"
                          , "Doris"
                          , "Eric"
                          , "Fred"
                          , "Gina"
                          , "Heather"
                          ]

-- | Authenticated example tree (prover version).
exampleTreeP :: Auth (Tree String 'P) 'P
exampleTreeP = toProver exampleTree

-- | Authenticated example tree (verifier version)
exampleTreeV :: Auth (Tree String 'V) 'V
exampleTreeV = toVerifier' exampleTree

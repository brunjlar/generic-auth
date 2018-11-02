{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Fix
Description : fixpoints of functors
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixpoints of functors and associated functions.
-}

module Data.Fix
    ( Fix
    , wrap
    , unwrap
    , cata
    , ana
    , hylo
    ) where

newtype Fix f = Wrap (f (Fix f))

deriving instance (forall a. Show a => Show (f a)) => Show (Fix f)
deriving instance (forall a. Eq a => Eq (f a)) => Eq (Fix f)
deriving instance (Eq (Fix f), forall a. Ord a => Ord (f a)) => Ord (Fix f)

wrap :: f (Fix f) -> Fix f
wrap = Wrap

unwrap :: Fix f -> f (Fix f)
unwrap (Wrap x) = x

-- | The /catamorphism/ for an @f@-algebra:
--
-- @
-- f (Fix f) ---- fmap (cata g) ----> f a
--
--     |                               |
--     | wrap                        g |
--     |                               |
--     V                               V
--
--   Fix f --------- cata g ---------> a
-- @
cata :: Functor f => (f a -> a) -> Fix f -> a
cata g = g . fmap (cata g) . unwrap

-- | The /anamorphism/ for an @f@-coalgebra:
--
-- @
-- f a ----- fmap (ana g) ----> f (Fix f)
--
--  A                               A
--  |                               |
--  | g                      unwrap |
--  |                               |
--
--  a  -------- ana g -------->   Fix f
-- @
ana :: Functor f => (a -> f a) -> a -> Fix f
ana g = wrap . fmap (ana g) . g

-- | A /hylomorphism/, an anamorphism followed by a catamorphism
-- (where the intermediate @'Fix' f@-structure is "fused" away):
--
-- @
--     --------------------- fmap (hylo g h) ---------------------
--    /                                                           \\
-- f a ----- fmap (ana g) ----> f (Fix f) ---- fmap (cata h) ----> f h
--
--  A                               A |                             |
--  |                               | | wrap                      h |
--  | g                      unwrap | |                             |
--  |                               | V                             V
--
--  a  -------- ana g -------->   Fix f --------- cata h ---------> b
--   \\                                                             /
--    -------------------------- hylo g h -------------------------
-- @
hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
hylo g h = h . fmap (hylo g h) . g

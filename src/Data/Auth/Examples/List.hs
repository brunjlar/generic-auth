{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Examples.List
Description : authenticated list example
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides the example of authenticated lists.
-}

module Data.Auth.Examples.List
    ( List (..)
    , fromList
    ) where

import Control.Monad
import Data.Auth

infixr 5 :>

-- | An authenticated list.
data List a = Nil | a :> Auth (List a)
    deriving (Show, Generic, Binary)

fromList :: (MonadAuth m, Binary a) => [a] -> m (List a)
fromList = foldM (\xs x -> (x :>) <$> auth xs) Nil . reverse

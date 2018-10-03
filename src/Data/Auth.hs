{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth
Description : authenticated data structures
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module reexports all definitions
necessary to work with authenticated data structures.
-}

module Data.Auth
    ( Binary
    , Generic
    , Auth
    , AuthError (..)
    , toHash
    , module Data.Auth.Hash
    , module Data.Auth.Monad
    ) where

import Data.Auth.Core
import Data.Auth.Hash
import Data.Auth.Monad
import GHC.Generics

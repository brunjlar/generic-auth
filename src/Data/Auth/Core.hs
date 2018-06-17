{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Core
Description : core functionality
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module reexports various internal modules
that provide this library's core functionality.
-}

module Data.Auth.Core
    ( module Data.Auth.Internal.Auth
    , module Data.Auth.Internal.Authenticatable
    , module Data.Auth.Internal.Monad
    , Binary
    ) where

import Data.Auth.Internal.Auth            hiding (authP, authV, unauthP,
                                           unauthV)
import Data.Auth.Internal.Authenticatable hiding (shallowCopy)
import Data.Auth.Internal.Monad
import Data.Binary                        (Binary)

module Data.Auth.Examples.Tree
    ( Tree (..)
    , example
    ) where

import Data.Auth.Core
import Data.Binary    (Binary)
import GHC.Generics

data Tree a =
      Tip a
    | Node (Auth (Tree a)) (Auth (Tree a))
    deriving (Show, Generic)

instance Binary a => Binary (Tree a) where

instance Authenticatable a => Authenticatable (Tree a) where

    shallowCopy (Tip a)    = Tip $ shallowCopy a
    shallowCopy (Node l r) = Node (shallowCopy l) (shallowCopy r)

example :: Tree String
example = Node
    (authP $ Node (authP $ Tip "Alice") (authP $ Tip "Bob"))
    (authP $ Node (authP $ Tip "Charlie") (authP $ Tip "Doris"))

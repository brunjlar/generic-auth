module Data.Auth.Examples.Tree
    (
    ) where

import Data.Auth.Core
import Data.Binary    (Binary)
import GHC.Generics

data Tree (r :: Role) a =
      Tip a
    | Node (Auth r (Tree r a)) (Auth r (Tree r a))
    deriving (Show, Generic)

instance Binary a => Binary (Tree 'Verifier a) where

instance Authenticatable a => Authenticatable (Tree r a) where

    shallowCopy (Tip a)    = Tip $ shallowCopy a
    shallowCopy (Node l r) = Node (shallowCopy l) (shallowCopy r)

example :: Tree 'Prover String
example = Node
    (authP $ Node (authP $ Tip "Alex") (authP $ Tip "Bob"))
    (authP $ Node (authP $ Tip "Alex") (authP $ Tip "Bob"))

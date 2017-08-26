module Data.Auth.Core.Role
    ( Role (..)
    , Shallow
    ) where

data Role = Prover | Verifier
    deriving (Show, Eq)

type family Shallow (a :: *) :: *

type instance Shallow (f r a) = f 'Verifier (Shallow a)
type instance Shallow (f r) = f 'Verifier
type instance Shallow [a] = [Shallow a]
type instance Shallow (a, b) = (Shallow a, Shallow b)
type instance Shallow (Either a b) = Either (Shallow a) (Shallow b)
type instance Shallow (Maybe a) = Maybe (Shallow a)

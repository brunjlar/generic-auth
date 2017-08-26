module Data.Auth.Core.Role
    ( Role (..)
    , Shallow
    ) where

data Role = Prover | Verifier
    deriving (Show, Eq)

type family Shallow (a :: *) :: * where
    Shallow (f r a) = f 'Verifier (Shallow a)
    Shallow (f r) = f 'Verifier
    Shallow [a] = [Shallow a]
    Shallow (a, b) = (Shallow a, Shallow b)
    Shallow (Either a b) = Either (Shallow a) (Shallow b)
    Shallow (Maybe a) = Maybe (Shallow a)
    Shallow a = a

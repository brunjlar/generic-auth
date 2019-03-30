{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Examples.List
Description : authenticated list example
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides the example of authenticated lists.
-}

module Examples.List
    ( List (..)
    , PortNumber
    , fromList
    , proverIO
    , verifierIO
    ) where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B8
import           Data.ByteString.Lazy   (ByteString, fromStrict, toStrict)
import           Network
import           System.IO              (BufferMode (..), Handle, hGetLine,
                                         hPrint, hPutStrLn, hSetBuffering)
import           Text.Read              (readMaybe)

import           Data.Auth

infixr 5 :>

-- | An authenticated list.
data List a i = Nil | a :> Auth (List a i) i
    deriving (Show, Generic)

deriving instance Serializable a => Serializable (List a 'P)
deriving instance Serializable a => Serializable (List a 'V)
deriving instance Deserializable a => Deserializable (List a 'P)
deriving instance Deserializable a => Deserializable (List a 'V)

-- | Converts a standard list into an authenticated list.
--
-- >>> fst $ runProver $ fromList "Haskell"
-- 'H' :> AuthP ('a' :> AuthP ('s' :> AuthP ('k' :> AuthP ('e' :> AuthP ('l' :> AuthP ('l' :> AuthP Nil))))))
fromList :: (MonadAuth i m, Serializable (List a i)) => [a] -> m (List a i)
fromList = foldr f $ return Nil
  where
    f x m = (x :>) <$> (m >>= auth)

data Command = Push Int | Pop
    deriving (Show, Read, Eq, Ord)

data Result = RPush | RPop (Maybe Int)
    deriving (Show, Read, Eq, Ord)

executeCommand :: ( MonadAuth i m
                  , Serializable (List Int i)
                  , Deserializable (List Int i)
                  )
               => Command
               -> Auth (List Int i) i
               -> m (Result, Auth (List Int i) i)
executeCommand (Push n) a = (RPush,) <$> auth (n :> a)
executeCommand Pop      a = do
    l <- unauth a
    return $ case l of
        Nil    -> (RPop Nothing, a)
        n :> b -> (RPop (Just n), b)

executeCommandLying :: MonadAuth 'P m
                    => Command
                    -> Auth (List Int 'P) 'P
                    -> m (Result, Auth (List Int 'P) 'P)
executeCommandLying (Push 42) a = executeCommand (Push 43) a
executeCommandLying c         a = executeCommand c a

-- | Starts the prover server.
proverIO :: Bool       -- ^ Indicates whether the prover should lie when pushing 42.
         -> PortNumber -- ^ The port to run on.
         -> IO ()
proverIO lie port = do
    putStrLn $ (if lie then "lying " else "") ++ "prover started"
    s <- listenOn $ PortNumber port
    putStrLn $ "prover listening on port " ++ show port
    (h, _, _) <- accept s
    hSetBuffering h LineBuffering
    putStrLn "accepted verifier connection"
    go h $ toProver nil
  where
    go :: Handle -> Auth (List Int 'P) 'P -> IO ()
    go h s = do
        putStrLn $ "STATE: " ++ show s
        c <- read <$> hGetLine h
        putStrLn $ "received command: " ++ show c
        let ((_, t), bs) = runProver $ (if lie then executeCommandLying
                                               else executeCommand)
                                       c s
        let bs' = toHex bs
        putStrLn $ "CERT: " ++ bs'
        hPutStrLn h bs'
        go h t

-- | Runs the verifier.
verifierIO :: PortNumber -- ^ The port to connect to.
           -> IO ()
verifierIO port = do
    putStrLn "verifier started"
    h <- connectTo "127.0.0.1" $ PortNumber port
    hSetBuffering h LineBuffering
    putStrLn $ "connected to prover on port " ++ show port
    go h $ toVerifier' nil
  where
    go :: Handle -> Auth (List Int 'V) 'V -> IO ()
    go h s = do
        putStrLn $ "STATE: " ++ show s
        putStrLn "enter command!"
        mc <- readMaybe <$> getLine
        case mc of
            Nothing -> putStrLn "commands are 'Push INT' or 'Pop'" >> go h s
            Just c  -> do
                hPrint h c
                bs' <- hGetLine h
                putStrLn $ "received cert: " ++ bs'
                let bs = fromHex bs'
                    e  = runVerifier (executeCommand c s) bs
                case e of
                    Left err       -> putStrLn $ "ERROR: " ++ show err
                    Right (res, t) -> putStrLn ("RESULT: " ++ show res) >> go h t

nil :: AuthM 'P (Auth (List Int 'P) 'P)
nil = auth Nil

toHex :: ByteString -> String
toHex = B8.unpack . B16.encode . toStrict

fromHex :: String -> ByteString
fromHex = fromStrict . fst . B16.decode . B8.pack

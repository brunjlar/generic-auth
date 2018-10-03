{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}

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
data List a = Nil | a :> Auth (List a)
    deriving (Show, Generic, Binary)

-- | Converts a standard list into an authenticated list.
--
-- >>> fst $ runProver $ fromList "Haskell"
-- 'H' :> AuthP ('a' :> AuthP ('s' :> AuthP ('k' :> AuthP ('e' :> AuthP ('l' :> AuthP ('l' :> AuthP Nil))))))
fromList :: (MonadAuth m, Binary a) => [a] -> m (List a)
fromList = foldr f $ return Nil
  where
    f x m = m >>= auth >>= return . (x :>)

data Command = Push Int | Pop
    deriving (Show, Read, Eq, Ord)

data Result = RPush | RPop (Maybe Int)
    deriving (Show, Read, Eq, Ord)

executeCommand :: MonadAuth m => Command -> Auth (List Int) -> m (Result, Auth (List Int))
executeCommand (Push n) a = (RPush,) <$> auth (n :> a)
executeCommand Pop      a = do
    l <- unauth a
    return $ case l of
        Nil    -> (RPop Nothing, a)
        n :> b -> (RPop (Just n), b)

executeCommandLying :: MonadAuth m => Command -> Auth (List Int) -> m (Result, Auth (List Int))
executeCommandLying (Push 42) a = executeCommand (Push 43) a
executeCommandLying c         a = executeCommand c a

proverIO :: Bool -> PortNumber -> IO ()
proverIO lie port = do
    putStrLn $ (if lie then "lying " else "") ++ "prover started"
    s <- listenOn $ PortNumber port
    putStrLn $ "prover listening on port " ++ show port
    (h, _, _) <- accept s
    hSetBuffering h LineBuffering
    putStrLn $ "accepted verifier connection"
    go h nilAuth
  where
    go :: Handle -> Auth (List Int) -> IO ()
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

verifierIO :: PortNumber -> IO ()
verifierIO port = do
    putStrLn "verifier started"
    h <- connectTo "127.0.0.1" $ PortNumber port
    hSetBuffering h LineBuffering
    putStrLn $ "connected to prover on port " ++ show port
    go h nilAuth
  where
    go :: Handle -> Auth (List Int) -> IO ()
    go h s = do
        putStrLn $ "STATE: " ++ show s
        putStrLn $ "enter command!"
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

nilAuth :: Auth (List Int)
nilAuth = fst $ runProver $ auth Nil

toHex :: ByteString -> String
toHex = B8.unpack . B16.encode . toStrict

fromHex :: String -> ByteString
fromHex = fromStrict . fst . B16.decode . B8.pack

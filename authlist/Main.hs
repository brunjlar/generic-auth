import System.Environment      (getArgs)
import Text.Read               (readMaybe)

import Data.Auth.Examples.List

main :: IO ()
main = do
    args <- getArgs
    case readArgs args of
        Just (LyingProver, p)  -> proverIO True p
        Just (HonestProver, p) -> proverIO False p
        Just (Verifier, p)     -> verifierIO p
        Nothing                -> putStrLn
            "USAGE: authlist prover PORT or authlist lying PORT or authlist verifier PORT"

data Role = HonestProver | LyingProver | Verifier deriving (Show, Eq, Ord)

readArgs :: [String] -> Maybe (Role, PortNumber)
readArgs [c, p] = do
    port <- readMaybe p
    role <- case c of
                "prover"   -> return HonestProver
                "lying"    -> return LyingProver
                "verifier" -> return Verifier
                _          -> Nothing
    return (role, port)
readArgs _      = Nothing

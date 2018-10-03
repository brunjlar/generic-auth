import System.Environment      (getArgs)

import Data.Auth.Examples.List

main :: IO ()
main = do
    [c, p] <- getArgs
    case c of
        "prover"   -> proverIO False $ read p
        "lying"    -> proverIO True $ read p
        "verifier" -> verifierIO $ read p
        _          -> putStrLn
            "USAGE: authlist prover PORT or authlist lying PORT or authlist verifier PORT"

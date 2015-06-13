import System.Environment
import Crypto

main = do
    args <- getArgs
    hexInput <- case args of
        [] -> do
            putStrLn "Enter the hex string to encode: "
            getLine
        (x : xs) -> return x
    putStrLn (bytesToBase64(hexToBytes hexInput))
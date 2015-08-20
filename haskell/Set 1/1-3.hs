import Crypto
import Utilities
import FreqAnalysis

import Data.Char
import Data.List
import Data.Word
import Data.Bits
import Data.Ord
import Control.Monad
import System.Environment

keys = [minBound ..] :: [Word8]
plaintexts inputBytes = map (flip decode inputBytes) keys
decode key = map (xor key)
bestPlaintext = maxBy score

decodeBytes = bytesToString . bestPlaintext . plaintexts
    
decodeHex = liftM decodeBytes . hexToBytes
decodeB64 = liftM decodeBytes . base64ToBytes

main = do
    args <- getArgs
    input <- case args of
        [] -> do
            putStrLn "Enter the hex string attempt to decrypt: "
            getLine
        (x : xs) -> return x
    putStrLn (case (decodeHex input) of
        Nothing -> "Error: Input must be a hex string"
        Just x -> x)
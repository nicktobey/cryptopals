import Crypto
import Utilities
import FreqAnalysis

import System.IO
import System.Environment
import Control.Monad
import Data.Bits
import Data.Word
import Data.List
import Data.Maybe

readLines :: Handle -> IO [String]
readLines handle = do
    eof <- hIsEOF handle
    if eof then
        return []
    else liftM2 (:) (hGetLine handle) (readLines handle)
       
decode key = map (xor key)

keys = [minBound ..] :: [Word8]

massDecode inputs =
    maxBy score (liftM2 decode keys inputs)

main = do
    hSetEncoding stdout latin1
    args <- getArgs
    handle <- case args of
        [] -> return stdin
        (x:xs) -> openFile x ReadMode
    lines <- readLines handle
    putStrLn $ bytesToString $ massDecode $ catMaybes $ map hexToBytes $ lines 
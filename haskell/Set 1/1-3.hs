import Crypto
import Data.Char
import Data.List
import Data.Word
import Data.Bits
import Data.Ord

isPrintable :: Char -> Bool
isPrintable x = (ord x >= 32) && (ord x <= 126)

scoreString :: (Word8 -> Int) -> [Word8] -> Int
scoreString scoreFunc input = sum ( map scoreFunc input )

asciiCheck :: Word8 -> Int
asciiCheck x = if isPrintable (chr (fromIntegral x)) then 1 else 0

massXor :: [Word8] -> Word8 -> [Word8]
massXor input key = map (xor key) input

--map chr (maximumBy (comparing (score asciiCheck)) (map (massXor (hexToBytes input)) keys))

decode input = plaintexts
    where
        keys = [(minBound :: Word8) ..] :: [Word8]
        inputBytes = hexToBytes input :: [Word8]
        key = maximumBy (comparing ((scoreString asciiCheck). (massXor inputBytes))) inputBytes :: Word8
        plaintexts = map (map (chr . fromIntegral) . (massXor inputBytes)) inputBytes :: [String]
        plaintextBytes = massXor inputBytes key :: [Word8]
        plaintext = map (chr . fromIntegral) plaintextBytes :: String
        score = scoreString asciiCheck plaintextBytes :: Int
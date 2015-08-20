module Crypto
( hexToBytes
, bytesToHex
, bytesToBase64
, base64ToBytes
, xorBytes
, xorHexes
) where

import Data.Word
import Data.List
import Data.Char
import Data.Bits
import Data.Traversable
import Control.Monad hiding (sequence)
import Control.Applicative
import Prelude hiding (sequence)

import Utilities

hexChars = "0123456789ABCDEF"
base64Chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']


hexToBytes :: String -> Maybe [Word8]
{- Converts a hex string into a byte array -}

hexToBytes hexes = hexToBytes' (map toUpper hexes)
hexToBytes' (char1 : char2 : xs) = do
    tail <- hexToBytes' xs
    byte1 <- char1 `elemIndex` hexChars
    byte2 <- char2 `elemIndex` hexChars
    return ((fromIntegral(byte1*16 + byte2) :: Word8) : tail)
hexToBytes' [_] = Nothing
hexToBytes' [] = Just []


{- Converts a byte array into a hex string -}
bytesToHex :: [Word8] -> String
bytesToHex (byte : bytes) = 
    let upperBits = byte `shiftR` 4
        lowerBits = byte .&. 0x0F
        encode ord = hexChars !! fromIntegral(ord)
    in encode upperBits : encode lowerBits : bytesToHex bytes
    
bytesToHex [] = []

{- Encodes a byte array with base64 encoding -}
bytesToBase64 :: [Word8] -> String
bytesToBase64 (byte1 : byte2 : byte3 : bytes) = char1 : char2 : char3 : char4 : bytesToBase64 bytes
  where
    (char1, char2, char3, char4) = bytesToBase64' byte1 byte2 byte3

bytesToBase64 [byte1, byte2] = [char1, char2, char3, '=']
  where
    (char1, char2, char3, _) = bytesToBase64' byte1 byte2 0
          
bytesToBase64 [byte1] = [char1, char2, '=', '=']
  where
    (char1, char2, _, _) = bytesToBase64' byte1 0 0 
          
bytesToBase64 [] = []

bytesToBase64' byte1 byte2 byte3 = (char1, char2, char3, char4)
  where
    encode ord = base64Chars !! fromIntegral(ord)
    char1 = encode(byte1 `shiftR` 2)
    char2 = encode(((byte1 .&. 0x03) `shiftL` 4) + byte2 `shiftR` 4)
    char3 = encode(((byte2 .&. 0x0F) `shiftL` 2) + byte3 `shiftR` 6)
    char4 = encode(byte3 .&. 0x3F)


{- Decodes a base64 encoded string back into a byte array -}
base64ToBytes :: String -> Maybe [Word8]

base64ToBytes [] = Just []
base64ToBytes [char1, char2, '=', '='] = do
    (byte1, _, _) <- base64ToBytes' char1 char2 'A' 'A'
    return [byte1]
  
base64ToBytes [char1, char2, char3, '='] = do
    (byte1, byte2, _) <- base64ToBytes' char1 char2 char3 'A'
    return [byte1, byte2]
  
base64ToBytes (char1 : char2 : char3 : char4 : chars) = do
    (byte1, byte2, byte3) <- base64ToBytes' char1 char2 char3 char4
    tail <- base64ToBytes chars
    return (byte1 : byte2 : byte3 : tail)
  
base64ToBytes chars = Nothing
  
base64ToBytes' :: Char -> Char -> Char -> Char -> Maybe (Word8, Word8, Word8)
base64ToBytes' char1 char2 char3 char4 = do
    let decode a = fmap fromIntegral (a `elemIndex` base64Chars)
    [bits1, bits2, bits3, bits4] <- (sequence . map decode) [char1, char2, char3, char4]
    let byte1 = (bits1 `shiftL` 2) + (bits2 `shiftR` 4)
        byte2 = ((bits2 .&. 0x0F) `shiftL` 4) + (bits3 `shiftR` 2)
        byte3 = ((bits3 .&. 0x03) `shiftL` 6) + bits4
    return (byte1, byte2, byte3)


xorHexes :: [Char] -> [Char] -> Maybe [Char]
xorHexes hex1 hex2 = do
    bytes1 <- hexToBytes hex1
    bytes2 <- hexToBytes hex2
    return ((bytesToHex . xorBytes bytes1) bytes2)


xorBytes = zipWith xor
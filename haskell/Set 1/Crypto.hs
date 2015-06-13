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

hexChars = "0123456789ABCDEF"
base64Chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']


{- Converts a hex string into a byte array -}
hexToBytes :: String -> [Word8]
hexToBytes hexes = hexToBytes' (map toUpper hexes) 0

hexToBytes' (char1 : char2 : xs) pos = case (maybeByte1, maybeByte2) of
    (Nothing, _) -> error ("Bad input at position " ++ show pos)
    (_, Nothing) -> error ("Bad input at position " ++ show (pos+1))
    ((Just byte1), (Just byte2)) -> fromIntegral(byte1*16 + byte2) :: Word8 : hexToBytes' xs (pos + 2)
    where maybeByte1 = char1 `elemIndex` hexChars
          maybeByte2 = char2 `elemIndex` hexChars
hexToBytes' (char1 : []) pos = error "Input of odd length"
hexToBytes' [] pos = []


{- Converts a byte array into a hex string -}
bytesToHex :: [Word8] -> String
bytesToHex (byte : bytes) = encode upperBits : encode lowerBits : bytesToHex bytes
    where encode ord = hexChars !! fromIntegral(ord)
          upperBits = byte `shiftR` 4
          lowerBits = byte .&. 0x0F
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
base64ToBytes :: String -> [Word8]
base64ToBytes chars = base64ToBytes' chars 0

base64ToBytes' :: String -> Int -> [Word8]
base64ToBytes' [] _ = []
base64ToBytes' [char1, char2, '=', '='] pos = [byte1]
  where (byte1, _, _) = base64ToBytes'' char1 char2 'A' 'A' pos
  
base64ToBytes' [char1, char2, char3, '='] pos = [byte1, byte2]
  where (byte1, byte2, _) = base64ToBytes'' char1 char2 char3 'A' pos
  
base64ToBytes' (char1 : char2 : char3 : char4 : chars) pos = byte1 : byte2 : byte3 : base64ToBytes' chars (pos+4)
  where (byte1, byte2, byte3) = base64ToBytes'' char1 char2 char3 char4 pos
  
base64ToBytes' chars pos = error ("Bad length of input" ++ (show chars))
  
base64ToBytes'' :: Char -> Char -> Char -> Char -> Int -> (Word8, Word8, Word8)
base64ToBytes'' char1 char2 char3 char4 pos = (byte1, byte2, byte3)
  where
    decode a pos = case (a `elemIndex` base64Chars) of
      Nothing -> error ("Bad input at position " ++ (show pos) ++ ": " ++ [a])
      Just byte -> fromIntegral(byte)
    bits1 = decode char1 pos
    bits2 = decode char2 (pos + 1)
    bits3 = decode char3 (pos + 2)
    bits4 = decode char4 (pos + 3)
    byte1 = (bits1 `shiftL` 2) + (bits2 `shiftR` 4)
    byte2 = ((bits2 .&. 0x0F) `shiftL` 4) + (bits3 `shiftR` 2)
    byte3 = ((bits3 .&. 0x03) `shiftL` 6) + bits4



xorHexes :: String -> String -> String
xorHexes hexes1 hexes2 = xorHexes' 0 (map toUpper hexes1) (map toUpper hexes2)
xorHexes' pos (hex1:hexes1) (hex2:hexes2) = case (maybeOrd1, maybeOrd2) of 
    (Nothing, _) -> error ("Bad first input at position " ++ show pos)
    (_, Nothing) -> error ("Bad second input at position " ++ show pos)
    ((Just ord1), (Just ord2)) -> hexChars !! (ord1 `xor` ord2) : xorHexes' (pos+1) hexes1 hexes2
    where
        maybeOrd1 = hex1 `elemIndex` hexChars
        maybeOrd2 = hex2 `elemIndex` hexChars
xorHexes' _ [] [] = []
xorHexes' _ [] (hex:hexes2) = error "Unequal lengths"
xorHexes' _ (hex:hexes1) [] = error "Unequal lengths"

xorBytes :: [Word8] -> [Word8] -> [Word8]
xorBytes [] [] = []
xorBytes [] _ = error "Unequal lengths"
xorBytes _ [] = error "Unequal lengths"
xorBytes (byte1:bytes1) (byte2:bytes2) = byte1 `xor` byte2 : xorBytes bytes1 bytes2
module FreqAnalysis
( score
, 
) where

import Data.Char
import Data.List
import Data.Word

isLowercase x = (x >= 'a') && (x <= 'z')

freqs = " cldhsrinoate"

asciiCheck :: Word8 -> Int
asciiCheck x = case ((chr . fromIntegral) x `elemIndex` freqs) of
    (Just index) -> index + 1
    Nothing -> if (isLowercase . chr . fromIntegral) x then 1 else 0

score = sum . map asciiCheck
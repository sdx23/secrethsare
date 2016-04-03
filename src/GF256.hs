module GF256 (
    GF256 (..)
) where

import Data.Word
import qualified Data.Bits as B
import qualified Data.Ratio as R

newtype GF256 = GF256 Word8 deriving (Eq, Show)

instance Enum GF256 where
        toEnum = GF256 . toEnum
        fromEnum (GF256 a) = fromEnum a

instance Num GF256 where
        (+) (GF256 a) (GF256 b) = GF256 $ B.xor a b
        (-) (GF256 a) (GF256 b) = GF256 $ B.xor a b
        (*) (GF256 0) _ = GF256 0
        (*) _ (GF256 0) = GF256 0
        (*) (GF256 a) (GF256 b) = GF256 $ foldl B.xor 0 $ filterMask b $ take 8 $ iterate shift a
            where 
                shift x | B.testBit x 7 = irredPoly `B.xor` B.shiftL x 1
                        | otherwise     = B.shiftL x 1
                irredPoly = 0x1d    -- careful: this is not 0x1b 
        abs      = id
        signum _ = 1
        fromInteger = GF256 . fromInteger

instance Fractional GF256 where
        recip 0 = error "no reciprocal for 0"
        recip a = head [x | x <- [0x01..0xff], x*a == 1]
        fromRational r = fromInteger (R.numerator r) / fromInteger (R.denominator r)

filterMask :: Word8 -> [a] -> [a]
filterMask _ [] = []
filterMask m (x:xs) | B.testBit m 0 = x:filterMask (B.shiftR m 1) xs
                    | otherwise     = filterMask (B.shiftR m 1) xs


module CRC24 (
    crc24
) where

import Data.Bits
import Data.Word
import Data.List (foldl')

poly :: Word32
poly = 0x1864cfb
ini :: Word32
ini = 0xb704ce

-- todo memorize
f :: Word32 -> Word32
f x   | testBit x 23  = shiftL x 1 `xor` poly
      | otherwise     = shiftL x 1

table :: Word8 -> Word32
table x = iterate f (shiftL ((toEnum . fromEnum) x) 16) !! 8

--crc24 d = foldl 

crc24 :: [Word8] -> [Word8]
crc24 ds = map (toEnum . fromEnum) [shiftR (c .&. 0xff0000) 16, shiftR (c .&. 0xff00) 8, c .&. 0xff]
    where c = foldl' h ini ds
          h i d = xor (shiftL i 8) $ table . (xor d) . toEnum . fromEnum $ 0xff .&. shiftR i 16

crc24h :: [Word8] -> Word32 -> Word32
crc24h [] i = i
crc24h (d:ds) i = crc24h ds ni
    where ni = xor tv (shiftL i 8)
          tv = table $ xor d $ (toEnum . fromEnum) $ 0xff .&. shiftR i 16


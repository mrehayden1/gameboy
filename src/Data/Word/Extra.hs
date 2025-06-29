module Data.Word.Extra where

import Data.Bits
import Data.Word

word16 :: Word8 -> Word8 -> Word16
word16 hi lo =
  let x = flip shift 8 . fromIntegral $ hi
  in x + fromIntegral lo

unWord16 :: Word16 -> (Word8, Word8)
unWord16 x =
  let hi = fromIntegral . flip shift (-8) $ x
      lo = fromIntegral x
  in (hi, lo)

swapNibbles :: Word8 -> Word8
swapNibbles =
  (.&.) <$> (`shift` (-4)) . (0xf0 .&.)
        <*> (`shift` 4) . (0x0f .&.)

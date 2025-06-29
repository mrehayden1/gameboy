module Data.Bits.Extra (
  assignBit
) where

import Data.Bits

assignBit :: Bits b => b -> Int -> Bool -> b
assignBit b n  True = b `setBit` n
assignBit b n False = b `clearBit` n

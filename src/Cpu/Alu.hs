{- HLINT ignore "Use camelCase" -}
module Cpu.Alu where

import Data.Bits
import Data.Bits.Extra
import Data.Bool
import Data.Word
import Data.Word.Extra

import Cpu.Core

-- Arithmetic/logic instructions
--
-- Instructions that have a destination (everything but compare) it is always
-- understood to be the accumulator, `a'.

-- 8-bit additions

add_n8 :: Cpu ()
add_n8 = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  add' x
  incPc 2
  syncCycles 8

add_HL :: Cpu ()
add_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  add' x
  incPc 1
  syncCycles 4

add_r8 :: Register -> Cpu ()
add_r8 r = do
  x <- peek r
  add' x
  incPc 1
  syncCycles 4

add' :: Word8 -> Cpu ()
add' x = do
  a' <- peek a
  let a'' = a' + x
  poke a a''
  poke zf $ a'' == 0
  poke nf False
  poke hf $ (a' .&. 0xf + x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' < x

-- 16-bit addition

add_hl_r16 :: Register16 -> Cpu ()
add_hl_r16 r = do
  hl' <- peek hl
  x <- peek r
  let hl'' = hl' + x
  poke hl hl''
  poke nf False
  poke hf $ (hl' .&. 0xf + x .&. 0xf) .&. 0x10 /= 0
  poke cf $ hl'' < x
  incPc 1
  syncCycles 8

-- TODO
add_sp_i8 :: Cpu ()
add_sp_i8 = do
  addr <- LocationInt8 . (+1) <$> peek pc
  x <- peek addr
  let x' = fromIntegral $ abs x
  pokeWith sp . bool (+ x') (subtract x') $ x < 0
  incPc 2
  syncCycles 16

-- 8-bit addition plus carry

adc_n8 :: Cpu ()
adc_n8 = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  adc' x
  incPc 2
  syncCycles 8

adc_r8 :: Register -> Cpu ()
adc_r8 r = do
  x <- peek r
  adc' x
  incPc 1
  syncCycles 4

adc_HL :: Cpu ()
adc_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  adc' x
  incPc 1
  syncCycles 8

adc' :: Word8 -> Cpu ()
adc' x = do
  c' <- bool 0 1 <$> peek cf
  let x' = x + c'
  a' <- peek a
  let a'' = a' + x'
  poke a a''
  poke zf $ a'' == 0x00
  poke nf False
  poke hf $ (a' .&. 0xf + x' .&. 0xf) .&. 0x10 /= 0x00
  poke cf $ a'' < x'

and_n8 :: Cpu ()
and_n8 = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  and' x
  incPc 2
  syncCycles 8

and_r8 :: Register -> Cpu ()
and_r8 r = do
  x <- peek r
  and' x
  incPc 1
  syncCycles 4

and_HL :: Cpu ()
and_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  and' x
  incPc 1
  syncCycles 8

and' :: Word8 -> Cpu ()
and' x = do
  a' <- (.&. x) <$> peek a
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf True
  poke cf False

compare_n8 :: Cpu ()
compare_n8 = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  compare' x
  incPc 2
  syncCycles 8

compare_r8 :: Register -> Cpu ()
compare_r8 r = do
  x <- peek r
  compare' x
  incPc 1
  syncCycles 4

compare_HL :: Cpu ()
compare_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  compare' x
  incPc 1
  syncCycles 8

compare' :: Word8 -> Cpu ()
compare' x = do
  a' <- peek a
  let a'' = a' - x
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'

complement_a :: Cpu ()
complement_a = do
  pokeWith a complement
  poke nf True
  poke hf True

or_n8 :: Cpu ()
or_n8 = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  or' x
  incPc 2
  syncCycles 8

or_r8 :: Register -> Cpu ()
or_r8 r = do
  x <- peek r
  or' x
  incPc 1
  syncCycles 4

or_HL :: Cpu ()
or_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  or' x
  incPc 1
  syncCycles 8

or' :: Word8 -> Cpu ()
or' x = do
  a' <- (.|. x) <$> peek a
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf False

xor_n8 :: Cpu ()
xor_n8 = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  xor' x
  incPc 2
  syncCycles 8

xor_r8 :: Register -> Cpu ()
xor_r8 r = do
  x <- peek r
  xor' x
  incPc 1
  syncCycles 4

xor_HL :: Cpu ()
xor_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  xor' x
  incPc 1
  syncCycles 8

xor' :: Word8 -> Cpu ()
xor' x = do
  a' <- xor x <$> peek a
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf False

sbc_n8 :: Cpu ()
sbc_n8 = do
  addr <- Location . (+1) <$> peek sp
  x <- peek addr
  sbc' x
  incPc 2
  syncCycles 8

sbc_r8 :: Register -> Cpu ()
sbc_r8 r = do
  x <- peek r
  sbc' x
  incPc 1
  syncCycles 4

sbc_HL :: Cpu ()
sbc_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  sbc' x
  incPc 1
  syncCycles 8

sbc' :: Word8 -> Cpu ()
sbc' x = do
  c' <- bool 0 1 <$> peek cf
  a' <- peek a
  let x' = x + c'
  let a'' = a' - x'
  poke a a''
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x' .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'

sub_n8 :: Cpu ()
sub_n8 = do
  addr <- Location . (+1) <$> peek sp
  x <- peek addr
  sub' x
  incPc 2
  syncCycles 8

sub_r8 :: Register -> Cpu ()
sub_r8 r = do
  x <- peek r
  sub' x
  incPc 1
  syncCycles 4

sub_HL :: Cpu ()
sub_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  sub' x
  incPc 1
  syncCycles 8

sub' :: Word8 -> Cpu ()
sub' x = do
  a' <- peek a
  let a'' = a' - x
  poke a a''
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > x


-- Increment / decrement
--
-- Increment or decrement the destination.

dec_HL :: Cpu ()
dec_HL = do
  addr <- Location <$> peek hl
  x <- pokeWith' addr . subtract $ 1
  poke zf $ x == 0
  poke nf True
  poke hf $ x == 0xff
  incPc 1
  syncCycles 12

dec_r8 :: Register -> Cpu ()
dec_r8 dst = do
  x <- subtract 1 <$> peek dst
  poke dst x
  poke zf $ x == 0
  poke nf True
  poke hf $ x == 0xff
  incPc 1
  syncCycles 4

dec_r16 :: Register16 -> Cpu ()
dec_r16 dst = do
  x <- subtract 1 <$> peek dst
  poke dst x
  incPc 1
  syncCycles 8

inc_HL :: Cpu ()
inc_HL = do
  addr <- Location <$> peek hl
  x <- pokeWith' addr (+1)
  poke zf $ x == 0
  poke nf False
  poke hf $ x == 0x10
  incPc 1
  syncCycles 12

inc_r8 :: Register -> Cpu ()
inc_r8 dst = do
  x <- (+1) <$> peek dst
  poke dst x
  poke zf $ x == 0
  poke nf False
  poke hf $ x == 0x10
  incPc 1
  syncCycles 4

inc_r16 :: Register16 -> Cpu ()
inc_r16 dst = do
  x <- (+1) <$> peek dst
  poke dst x
  incPc 1
  syncCycles 8

-- Bitwise operations

reset_b_r8 :: Int -> Register -> Cpu ()
reset_b_r8 b' r = do
  pokeWith r (`clearBit` b')
  incPc 2
  syncCycles 8

reset_b_HL :: Int -> Cpu ()
reset_b_HL b' = do
  hl' <- peek hl
  pokeWith (Location hl') (`clearBit` b')
  incPc 2
  syncCycles 16

-- rotateL_a — More efficient left rotate for the accumulator only.
--
--  Always sets zero flag to false unlike other rotateX_x functions.
--
rotateL_a :: Cpu ()
rotateL_a = do
  a' <- flip rotate 1 <$> peek a
  poke a a'
  poke zf False
  poke nf False
  poke hf False
  poke cf $ a' `testBit` 0
  incPc 1
  syncCycles 4

rotateL_r8 :: Register -> Cpu ()
rotateL_r8 r = do
  x <- pokeWith' r $ flip rotate 1
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ x `testBit` 0
  incPc 2
  syncCycles 8

rotateL_HL :: Cpu ()
rotateL_HL = do
  l' <- Location <$> peek hl
  x <- pokeWith' l' . flip rotate $ 1
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ x `testBit` 7
  incPc 2
  syncCycles 16

-- rotateLC_a — More efficient left rotate through carry for the accumulator
--   only.
--
--  Always sets zero flag to false unlike other rotateX_x functions.
--
rotateLC_a :: Cpu ()
rotateLC_a = do
  c' <- peek cf
  a' <- peek a
  let a'' = flip (`assignBit` 0) c' . shift a' $ 1
  poke a a''
  poke zf False
  poke nf False
  poke hf False
  poke cf . testBit a' $ 7
  incPc 1
  syncCycles 4

rotateLC_r8 :: Register -> Cpu ()
rotateLC_r8 r = do
  c' <- peek cf
  x <- peek r
  let x' = flip (`assignBit` 0) c' . shift x $ 1
  poke r x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf . testBit x $ 7
  incPc 2
  syncCycles 8

rotateLC_HL :: Cpu ()
rotateLC_HL = do
  c' <- peek cf
  addr <- Location <$> peek hl
  x <- peek addr
  let x' = flip (`assignBit` 0) c' . shift x $ 1
  poke addr x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf . testBit x $ 7
  incPc 2
  syncCycles 16

-- rotateR_a — More efficient right rotate for the accumulator only.
--
--  Always sets zero flag to false unlike other rotateX_x functions.
--
rotateR_a :: Cpu ()
rotateR_a = do
  a' <- pokeWith' a $ flip rotate (-1)
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf $ a' `testBit` 7
  incPc 1
  syncCycles 4

rotateR_r8 :: Register -> Cpu ()
rotateR_r8 r = do
  x <- pokeWith' r $ flip rotate (-1)
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ x `testBit` 7
  incPc 2
  syncCycles 8

rotateR_HL :: Cpu ()
rotateR_HL = do
  l' <- Location <$> peek hl
  x <- pokeWith' l' $ flip rotate (-1)
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ x `testBit` 7
  incPc 2
  syncCycles 16

-- rotateRC_a — More efficient right rotate through carry for the accumulator
--   only.
--
--  Always sets zero flag to false unlike other rotateX_x functions.
--
rotateRC_a :: Cpu ()
rotateRC_a = do
  c' <- peek cf
  a' <- peek a
  let a'' = flip (`assignBit` 7) c' . shift a' $ -1
  poke a a''
  poke zf False
  poke nf False
  poke hf False
  poke cf . testBit a' $ 0
  incPc 1
  syncCycles 4

rotateRC_r8 :: Register -> Cpu ()
rotateRC_r8 r = do
  c' <- peek cf
  x <- peek r
  let x' = flip (`assignBit` 7) c' . shift x $ -1
  poke r x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf . testBit x $ 0
  incPc 2
  syncCycles 8

rotateRC_HL :: Cpu ()
rotateRC_HL = do
  c' <- peek cf
  addr <- Location <$> peek hl
  x <- peek addr
  let x' = flip (`assignBit` 7) c' . shift x $ -1
  poke addr x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf . testBit x $ 0
  incPc 2
  syncCycles 16

set_b_r8 :: Int -> Register -> Cpu ()
set_b_r8 b' r = do
  pokeWith r (`setBit` b')
  incPc 2
  syncCycles 8

set_b_HL :: Int -> Cpu ()
set_b_HL b' = do
  l' <- Location <$> peek hl
  pokeWith l' (`setBit` b')
  incPc 2
  syncCycles 16

shiftLeftArithmetically_r8 :: Register -> Cpu ()
shiftLeftArithmetically_r8 r = do
  r' <- peek r
  let x = shift r' 1
  poke r x
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ testBit r' 7
  incPc 2
  syncCycles 8

shiftLeftArithmetically_HL :: Cpu ()
shiftLeftArithmetically_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  let x' = shift x 1
  poke addr x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf $ testBit x 7
  incPc 2
  syncCycles 8

shiftRightArithmetically_r8 :: Register -> Cpu ()
shiftRightArithmetically_r8 r = do
  r' <- peek r
  let x = shift r' $ -1
  poke r x
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ testBit r' 0
  incPc 2
  syncCycles 8

shiftRightArithmetically_HL :: Cpu ()
shiftRightArithmetically_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  let x' = shift x $ -1
  poke addr x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf $ testBit x 0
  incPc 2
  syncCycles 8

shiftRightLogical_r8 :: Register -> Cpu ()
shiftRightLogical_r8 r = do
  r' <- peek r
  let x = shift r' (-1)
  poke r x
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf $ r' `testBit` 0
  incPc 2
  syncCycles 8

shiftRightLogical_HL :: Cpu ()
shiftRightLogical_HL = do
  l' <- Location <$> peek hl
  x <- peek l'
  let x' = shift x (-1)
  poke l' x'
  poke zf $ x' == 0
  poke nf False
  poke hf False
  poke cf $ x `testBit` 0
  incPc 2
  syncCycles 8

swap_r8 :: Register -> Cpu ()
swap_r8 r = do
  x <- pokeWith' r swapNibbles
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf False
  incPc 2
  syncCycles 8

swap_HL :: Cpu ()
swap_HL = do
  l' <- Location <$> peek hl
  x <- pokeWith' l' swapNibbles
  poke zf $ x == 0
  poke nf False
  poke hf False
  poke cf False
  incPc 2
  syncCycles 16

test_b_r8 :: Int -> Register -> Cpu ()
test_b_r8 b' r = do
  z' <- not . (`testBit` b') <$> peek r
  poke zf z'
  poke nf False
  poke hf True
  incPc 2
  syncCycles 8

test_b_HL :: Int -> Cpu ()
test_b_HL b' = do
  l' <- Location <$> peek hl
  z' <- not . (`testBit` b') <$> peek l'
  poke zf z'
  poke nf False
  poke hf True
  incPc 2
  syncCycles 16

-- Carry flag instructions

complement_cf :: Cpu ()
complement_cf = do
  poke nf False
  poke hf False
  pokeWith cf not
  incPc 1
  syncCycles 4

set_cf :: Cpu ()
set_cf = do
  poke nf False
  poke hf False
  poke cf True
  incPc 1
  syncCycles 4

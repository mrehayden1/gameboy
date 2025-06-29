{- HLINT ignore "Use camelCase" -}
module Cpu.Load (
  ld_HL_n8,
  ld_a_HL',
  ld_HL'_a,
  ld_a_R16,
  ld_r8_n8,
  ld_r16_n16,
  ld_r8_r8,
  ld_R16_r8,
  ld_r8_R16,
  ld_N16_sp,
  ld_N16_a,
  ld_a_N16,
  ldh_N8_a,
  ldh_a_N8,
  ldh_C_a,
  ldh_a_C
) where

import Cpu.Core

ld_HL_n8 :: Cpu ()
ld_HL_n8 = do
  hlAddr <- Location <$> peek hl
  n8Addr <- Location . (+1) <$> peek pc
  peek n8Addr >>= poke hlAddr
  incPc 2
  syncCycles 12

ld_a_HL' :: IncDec -> Cpu ()
ld_a_HL' opr = do
  addr <- Location <$> peek hl
  pokeWith hl $ incDec opr
  peek addr >>= poke a
  incPc 1
  syncCycles 8

ld_HL'_a :: IncDec -> Cpu ()
ld_HL'_a opr = do
  addr <- Location <$> peek hl
  pokeWith hl $ incDec opr
  peek a >>= poke addr
  incPc 1
  syncCycles 8

ld_a_R16 :: Register16 -> Cpu ()
ld_a_R16 r16 = do
  addr <- Location <$> peek r16
  x <- peek addr
  poke a x
  incPc 1
  syncCycles 8

ld_r8_n8 :: Register -> Cpu ()
ld_r8_n8 dst = do
  addr <- Location . (+1) <$> peek pc
  x <- peek addr
  poke dst x
  incPc 2
  syncCycles 8

ld_r16_n16 :: Register16 -> Cpu ()
ld_r16_n16 dst = do
  addr <- Location16 . (+1) <$> peek pc
  peek addr >>= poke dst
  incPc 3
  syncCycles 12

ld_r8_r8 :: Register -> Register -> Cpu ()
ld_r8_r8 dst src = do
  x <- peek src
  poke dst x
  incPc 1
  syncCycles 4

ld_R16_r8 :: Register16 -> Register -> Cpu ()
ld_R16_r8 ptr src = do
  l' <- Location <$> peek ptr
  x <- peek src
  poke l' x
  incPc 1
  syncCycles 8

ld_r8_R16 :: Register -> Register16 -> Cpu ()
ld_r8_R16 dst ptr = do
  l' <- Location <$> peek ptr
  x <- peek l'
  poke dst x
  incPc 1
  syncCycles 8

ld_N16_sp :: Cpu ()
ld_N16_sp = do
  n16Addr <- Location16 . (+1) <$> peek pc
  addr <- Location16 <$> peek n16Addr
  peek sp >>= poke addr
  incPc 3
  syncCycles 20

ld_N16_a :: Cpu ()
ld_N16_a = do
  n16Addr <- Location16 . (+1) <$> peek pc
  addr <- Location <$> peek n16Addr
  peek a >>= poke addr
  incPc 3
  syncCycles 16

ld_a_N16 :: Cpu ()
ld_a_N16 = do
  n16Addr <- Location16 . (+1) <$> peek pc
  addr <- Location <$> peek n16Addr
  peek addr >>= poke a
  incPc 3
  syncCycles 16

ldh_N8_a :: Cpu ()
ldh_N8_a = do
  n8Addr <- Location . (+1) <$> peek pc
  addr <- Location . (+ 0xff00) . fromIntegral <$> peek n8Addr
  peek a >>= poke addr
  incPc 2
  syncCycles 8

ldh_a_N8 :: Cpu ()
ldh_a_N8 = do
  n8Addr <- Location . (+1) <$> peek pc
  addr <- Location . (+ 0xff00) . fromIntegral <$> peek n8Addr
  peek addr >>= poke a
  incPc 2
  syncCycles 8

ldh_C_a :: Cpu ()
ldh_C_a = do
  addr <- Location . (+ 0xff00) . fromIntegral <$> peek c
  peek a >>= poke addr
  incPc 2
  syncCycles 8

ldh_a_C :: Cpu ()
ldh_a_C = do
  addr <- Location . (+ 0xff00) . fromIntegral <$> peek c
  peek addr >>= poke a
  incPc 2
  syncCycles 8

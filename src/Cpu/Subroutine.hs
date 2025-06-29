module Cpu.Subroutine (
  pop_r16,
  push_r16,

  call_n16,
  callCc_n16,

  rst,

  ret,
  retCc
) where

import Data.Bool
import Data.Word

import Cpu.Core

-- Stack operations

pop_r16 :: Register16 -> Cpu ()
pop_r16 r16 = do
  addr <- Location16 <$> peek sp
  peek addr >>= poke r16
  pokeWith sp (+2)
  incPc 1
  syncCycles 12

push_r16 :: Register16 -> Cpu ()
push_r16 r16 = do
  addr <- Location16 <$> pokeWith' sp (subtract 2)
  peek r16 >>= poke addr
  incPc 1
  syncCycles 16

-- Subroutines

call_n16 :: Cpu ()
call_n16 = do
  -- Push the next instruction address to the stack
  pc' <- peek pc
  spAddr <- Location16 <$> pokeWith' sp (subtract 2)
  poke spAddr $ pc' + 3
  -- Jump to the given destination
  jmpDest <- peek . Location16 . (+1) $ pc'
  poke pc jmpDest
  syncCycles 24

callCc_n16 :: Bool -> Flag -> Cpu ()
callCc_n16 invert cc = do
  cc' <- bool id not invert <$> peek cc
  if cc'
    then call_n16
    else do
      incPc 3
      syncCycles 12

rst :: Word16 -> Cpu ()
rst addr = do
  -- Push the next instruction address to the stack
  pc' <- peek pc
  spAddr <- Location16 <$> pokeWith' sp (subtract 2)
  poke spAddr $ pc' + 1
  -- Jump to the given destination
  jmpDest <- peek . Location16 $ addr
  poke pc jmpDest
  syncCycles 16

ret :: Cpu ()
ret = do
  ret'
  incPc 1
  syncCycles 16

retCc :: Bool -> Flag -> Cpu ()
retCc invert cc = do
  cc' <- bool id not invert <$> peek cc
  if cc'
    then do
      ret'
      incPc 1
      syncCycles 20
    else do
      incPc 2
      syncCycles 8

ret' :: Cpu ()
ret' = do
  addr <- Location16 <$> peek sp
  peek addr >>= poke pc
  pokeWith sp (+2)

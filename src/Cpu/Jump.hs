{- HLINT ignore "Use camelCase" -}
module Cpu.Jump where

import Data.Bool

import Cpu.Core

-- Absolute jumps

jp_HL :: Cpu ()
jp_HL = do
  addr <- Location16 <$> peek hl
  x <- peek addr
  poke pc x
  syncCycles 4

jp_n16 :: Cpu ()
jp_n16 = do
  addr <- Location16 . (+1) <$> peek pc
  x <- peek addr
  poke pc x
  syncCycles 16

jpCc_n16 :: Bool -> Flag -> Cpu ()
jpCc_n16 invert cc = do
  cc' <- bool id not invert <$> peek cc
  if cc'
    then jp_n16
    else do
      incPc 3
      syncCycles 12

-- Relative jumps

jr :: Cpu ()
jr =  do
  addr <- LocationInt8 . (+1) <$> peek pc
  x <- peek addr
  let x' = fromIntegral $ abs x
  pokeWith pc . bool (+ x') (subtract x') $ x < 0
  syncCycles 12

jrCc :: Bool -> Flag -> Cpu ()
jrCc invert cc = do
  cc' <- bool id not invert <$> peek cc
  if cc'
    then jr
    else do
      incPc 2
      syncCycles 8

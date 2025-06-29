module Cpu.Interrupt (
  disableInterrupts,
  enableInterrupts
) where

import Control.Lens
import Control.Monad.State

import Cpu.Core

disableInterrupts :: Cpu ()
disableInterrupts = do
  modify $ ime .~ Right False
  incPc 1
  syncCycles 4

-- Enabling the IME flag is always delayed by one CPU instruction.
enableInterrupts :: Cpu ()
enableInterrupts = do
  modify $ ime .~ Left ()
  incPc 1
  syncCycles 4

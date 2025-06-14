module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import GHC.IO.Handle
import GHC.IO.StdHandles

import Cpu

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  runCpu $ do
    forever $ do
      c <- liftIO $ do
        putStrLn "Enter command:"
        putStrLn "[c] Current instruction. [r] Print registers. [s] Step. [m] Print memory."
        c <- liftIO getChar
        putStrLn ""
        return c
      case c of
        'c' -> printInstruction
        'C' -> printInstruction
        'm' -> printMemory
        'M' -> printMemory
        'r' -> printRegisters
        'R' -> printRegisters
        's' -> step
        'S' -> step
        _   -> return ()

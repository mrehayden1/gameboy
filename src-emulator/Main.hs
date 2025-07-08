module Main (
  main
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import GHC.Base
import System.Environment
import System.Exit
import Text.Printf

import Cpu
import Cpu.Core
import Rom

main :: IO ()
main = do
  cpu0 <- initCpuS
  let mem = mem_ cpu0

  args <- getArgs

  when (null args) $ do
    putStrLn "No ROM specified."
    putStrLn "Usage: debugger <rom file>"
    exitFailure

  let romPath = head args

  putStrLn . printf "Loading ROM \"%s\"" $ romPath
  loadRom romPath mem

  -- Run the ROM directly.
  putStrLn "Executing ROM..."
  flip evalCpu cpu0 . forever $ do
    step
    readSerial >>= traverse (liftIO . putChar)

readSerial :: Cpu (Maybe Char)
readSerial = do
  sc' <- peek sc
  if testBit sc' 7
    then do
      sb' <- peek sb
      poke sc $ sc' .&. 0x7f
      return . Just . unsafeChr . fromIntegral $ sb'
    else return Nothing

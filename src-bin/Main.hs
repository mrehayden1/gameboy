module Main (
  main
) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Array.IO
import Data.Bits
import Data.Bool
import qualified Data.Text as T
import Data.Word
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Vty
import Text.Printf

import Cpu.Core
import Rom

type Widget t m =
  ( Adjustable t m
  , HasLayout t m
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocus t m
  , HasFocusReader t m
  , HasTheme t m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  )

main :: IO ()
main = do
  cpu0 <- initCpuS
  let mem = mem_ cpu0

  putStrLn "Loading Blargg's CPU instruction test ROM"
  loadRom "roms/blargg/cpu_instrs/cpu_instrs.gb" mem

  mainWidget $ initManager_ $ do
    col $ do
      grout (fixed 1) . text $ "LR35902 Emulator Debugger"
      row $ do
        grout flex blank
        grout (fixed 49) $
          boxTitle (pure roundedBoxStyle) "Current Instruction" blank
        grout (fixed 48) $
          col $ do
            grout (fixed 8) $
              boxTitle (pure roundedBoxStyle) "8-bit Registers" registers
            grout flex $
              boxTitle (pure roundedBoxStyle) "16-bit Registers" registers16
        memoryBox mem
        grout flex blank
      grout (fixed 1) $ text "Status bar"
    ctrlc

registers16 :: forall t m. Widget t m => m ()
registers16 = row $ do
  t <- theme
  grout (fixed 2) blank
  col $ do
    grout (fixed 1) . row $ do
      grout flex . richText (RichTextConfig . fmap (`V.withStyle` V.dim) $ t)
        $ "           ZNHC"
    grout (fixed 1) . row $ do
      grout flex $ register "AF"
    grout (fixed 1) . row $ do
      grout flex $ register "BC"
    grout (fixed 1) . row $ do
      grout flex $ register "DE"
    grout (fixed 1) . row $ do
      grout flex $ register "HL"
  grout (fixed 2) blank
 where
  register :: String -> m ()
  register n = do
    t <- theme
    row $ do
      grout (fixed 4) . richText (RichTextConfig t)
        . pure . T.pack . printf "%s:  " $ n
      grout flex . richText (RichTextConfig . fmap (`V.withStyle` V.bold) $ t)
        . pure . T.pack . printf "%02x %02x  %s" (0xff :: Word8) (0x00 :: Word8)
        . binaryStr $ (0xff00 :: Word16)
  binaryStr n =
    fmap digit [15,14,13,12] <> " " <> fmap digit [11,10,9,8] <> " "
      <> fmap digit [7,6,5,4] <> " " <> fmap digit [3,2,1,0]
   where
    digit = bool '0' '1' . testBit n

registers :: forall t m. Widget t m => m ()
registers = row $ do
  t <- theme
  grout (fixed 2) blank
  col $ do
    grout (fixed 1) . row $ do
      grout flex blank
      grout flex . richText (RichTextConfig . fmap (`V.withStyle` V.dim) $ t)
        $ "       ZNHC"
    grout (fixed 1) . row $ do
      grout flex $ register "A"
      grout flex $ register "F"
    grout (fixed 1) . row $ do
      grout flex $ register "B"
      grout flex $ register "C"
    grout (fixed 1) . row $ do
      grout flex $ register "D"
      grout flex $ register "E"
    grout (fixed 1) . row $ do
      grout flex $ register "H"
      grout flex $ register "L"
  grout (fixed 2) blank
 where
  register :: String -> m ()
  register n = do
    t <- theme
    row $ do
      grout (fixed 3) . richText (RichTextConfig t)
        . pure . T.pack . printf "%s: " $ n
      grout flex . richText (RichTextConfig . fmap (`V.withStyle` V.bold) $ t)
        . pure . T.pack . printf "%02x  %s" (255 :: Word8)
        . binaryStr $ (255 :: Word8)

  binaryStr n =
    fmap digit [7,6,5,4] <> " " <> fmap digit [3,2,1,0]
   where
    digit = bool '0' '1' . testBit n

memoryBox :: Widget t m => IOUArray Word16 Word8 -> m ()
memoryBox mem = do
  postBuild <- getPostBuild
  i <- input
  memBaseE <- fmap (updated . fmap (* 0x0010)) . foldDyn ($) 0 . fforMaybe i
    $ \case
      V.EvKey (V.KChar 'k') _ -> Just (\x -> if x <=     0 then x else x - 1)
      V.EvKey (V.KChar 'j') _ -> Just (\x -> if x >= 0xfff then x else x + 1)
      _                       -> Nothing
  memBlocks <- performEvent . fmap (formatMem mem)
    $ leftmost [0x0000 <$ postBuild, memBaseE]
  grout (fixed 49) $ boxTitle (pure roundedBoxStyle) "Memory Browser" $
    void . networkHold blank $ memBlocks

formatMem :: (MonadIO m', Widget t m) => IOUArray Word16 Word8 -> Word16 -> m' (m ())
formatMem mem base = do
  blocks <- forM [base, base+0x0010..base+0x0300] $ \block -> do
    let p = V.string V.defAttr . printf " %04x: " $ block
        blocks = [block, block+0x0002..block+0x000f]
    s <- fmap V.horizCat . forM blocks $ \group -> do
      let groups = [group, group+0x001]
      ds <- fmap V.horizCat . forM groups $
              fmap formatByte . liftIO . readArray mem
      return $ ds V.<|> V.string V.defAttr " "
    return $ p V.<|> s
  return . tellImages . pure . pure . V.vertCat $ blocks
 where
  formatByte :: Word8 -> V.Image
  formatByte x
    | x == 0    = V.string (V.defAttr `V.withStyle` V.dim) . printf "%02x" $ x
    | otherwise = V.string (V.defAttr `V.withStyle` V.bold) . printf "%02x" $ x

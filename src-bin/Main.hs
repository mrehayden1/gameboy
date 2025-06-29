module Main (
  main
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Array.IO
import Data.Bits
import Data.Bool
import qualified Data.Text as T
import Data.Word
import GHC.Base
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Vty
import Text.Printf

import Cpu
import Cpu.Core
import Debugger.Instructions
import Debugger.Widget
import Rom

main :: IO ()
main = do
  cpu0 <- initCpuS
  let mem = mem_ cpu0

  putStrLn "Loading ROM"
  --loadRom "roms/blargg/cpu_instrs/cpu_instrs.gb" mem
  --loadRom "roms/blargg/cpu_instrs/individual/10-bit ops.gb" mem
  loadRom "roms/count.gb" mem

  -- Run the ROM directly.
  --void . runCpu (forever step) $ cpu0

  -- Debug the ROM.
  mainWidget $ initManager_ $ app cpu0

app :: Widget t m => CpuS -> m (Event t ())
app cpu0 = do
  let mem = mem_ cpu0

  i <- input
  let stepInputE = void . fforMaybe i $ \case
        V.EvKey (V.KChar 's') _ -> Just ()
        _                       -> Nothing

  rec
    cpuStepE <- performEvent . fmap stepCpu $
      current cpuState <@ leftmost [stepInputE]
    cpuState <- holdDyn cpu0 . fmap snd $ cpuStepE

  let serialE = fmapMaybe fst cpuStepE
  serial <- foldDyn ((++) . pure) "" serialE

  col $ do
    grout (fixed 1) . text $ "LR35902 Emulator Debugger"
    row $ do
      grout flex blank
      grout (fixed 48) $
        col $ do
          grout (fixed 8) $
            boxTitle (pure roundedBoxStyle) "8-bit Registers" $
              registers cpuState
          grout (fixed 11) $
            boxTitle (pure roundedBoxStyle) "16-bit Registers" $
              registers16 cpuState
          grout (fixed 5) $
            boxTitle (pure roundedBoxStyle) "Currrent Instruction" $
              Debugger.Instructions.widget cpu0 cpuState
          grout flex $
            boxTitle (pure roundedBoxStyle) "Serial output" $
              text . fmap T.pack . current $ serial
      grout (fixed 49) $
        boxTitle (pure roundedBoxStyle) "Memory Browser" $ memory mem
      grout flex blank
    grout (fixed 1) $ text "Commands: [s] Step, [r] Continue, [Ctrl-c] Quit"

  -- Exit on Ctrl-c
  ctrlc

stepCpu :: MonadIO m => CpuS -> m (Maybe Char, CpuS)
stepCpu s = liftIO . flip runCpu s $ do
  Cpu.step
  readSerial

readSerial :: Cpu (Maybe Char)
readSerial = do
  sc' <- peek sc
  if testBit sc' 7
    then do
      sb' <- peek sb
      poke sc $ sc' .&. 0x7f
      return . Just . unsafeChr . fromIntegral $ sb'
    else return Nothing

registers :: forall t m. Widget t m => Dynamic t CpuS -> m ()
registers cpu = row $ do
  grout (fixed 2) blank
  col $ do
    grout (fixed 1) blank
    row $ do
      grout (fixed 19) . col $ do
        forM_ [("A", a_), ("B", b_), ("D", d_), ("H", h_)] $ \(n, g) ->
          grout (fixed 1) . row $ do
            grout flex . register n $ g <$> current cpu
      grout (fixed 4) blank
      grout (fixed 19) . col $ do
        forM_ [("F", f_), ("C", c_), ("E", e_), ("L", l_)] $ \(n, g) ->
          grout (fixed 1) . row $ do
            grout flex $ register n $ g <$> current cpu
  grout (fixed 2) blank
 where
  register :: String -> Behavior t Word8 -> m ()
  register n val = do
    t <- theme
    row $ do
      grout (fixed 3) . richText (RichTextConfig t)
        . pure . T.pack . printf "%s: " $ n
      grout flex . richText (RichTextConfig . fmap (`V.withStyle` V.bold) $ t)
        . fmap (T.pack . printf "%02x") $ val
      grout (fixed 9) . richText (RichTextConfig t)
        . fmap (T.pack . printf "%s" . binaryStr) $ val

  binaryStr n =
    fmap digit [7,6,5,4] <> " " <> fmap digit [3,2,1,0]
   where
    digit = bool '0' '1' . testBit n

registers16 :: forall t m. Widget t m => Dynamic t CpuS -> m ()
registers16 cpu = row $ do
  grout (fixed 2) blank
  col $ do
    grout (fixed 1) . row $ blank
    forM_ [("AF", (^. af')), ("BC", (^. bc')), ("DE", (^. de')), ("HL", (^. hl'))] $ \(n, g) ->
      grout (fixed 1) . row $ do
        grout flex . register n $ g <$> current cpu
    grout (fixed 1) blank
    forM_ [("PC", pc_), ("SP", sp_)] $ \(n, g) ->
      grout (fixed 1) . row $ do
        grout flex . register n $ g <$> current cpu
  grout (fixed 2) blank
 where
  register :: String -> Behavior t Word16 -> m ()
  register n val = do
    t <- theme
    row $ do
      grout (fixed 5) . richText (RichTextConfig t)
        . pure . T.pack . printf "%s:  " $ n
      grout flex . richText (RichTextConfig . fmap (`V.withStyle` V.bold) $ t)
        . fmap (T.pack . printf "%04x") $ val
      grout (fixed 19) . richText (RichTextConfig . fmap (`V.withStyle` V.bold) $ t)
        . fmap (T.pack . printf "%s" . binaryStr) $ val
  binaryStr n =
    fmap digit [15,14,13,12] <> " " <> fmap digit [11,10,9,8] <> " "
      <> fmap digit [7,6,5,4] <> " " <> fmap digit [3,2,1,0]
   where
    digit = bool '0' '1' . testBit n

memory :: Widget t m => IOUArray Word16 Word8 -> m ()
memory mem = do
  postBuild <- getPostBuild
  t <- theme
  i <- input
  memBaseE <- fmap (updated . fmap (* 0x0010)) . foldDyn ($) 0 . fforMaybe i
    $ \case
      V.EvKey (V.KChar 'k') _ -> Just (\x -> if x <=     0 then x else x - 1)
      V.EvKey (V.KChar 'j') _ -> Just (\x -> if x >= 0xfff then x else x + 1)
      _                       -> Nothing
  memBlocks <- performEvent $
    fmap (formatMem mem) t <@> leftmost [0x0000 <$ postBuild, memBaseE]
  void . networkHold blank $ memBlocks

formatMem :: (MonadIO m', Widget t m) => IOUArray Word16 Word8 -> V.Attr -> Word16 -> m' (m ())
formatMem mem attr base = do
  blocks <- forM [base, base+0x0010..base+0x0400] $ \block -> do
    let p = V.string attr . printf " %04x: " $ block
        blocks = [block, block+0x0002..block+0x000f]
    s <- fmap V.horizCat . forM blocks $ \group -> do
      let groups = [group, group+0x001]
      ds <- fmap V.horizCat . forM groups $
              fmap formatByte . liftIO . readArray mem
      return $ ds V.<|> V.string attr " "
    return $ p V.<|> s
  return . tellImages . pure . pure . V.vertCat $ blocks
 where
  formatByte :: Word8 -> V.Image
  formatByte x
    | x == 0    = V.string (attr `V.withStyle` V.dim) . printf "%02x" $ x
    | otherwise = V.string (attr `V.withStyle` V.bold) . printf "%02x" $ x

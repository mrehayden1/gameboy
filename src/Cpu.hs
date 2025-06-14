{- HLINT ignore "Use camelCase" -}
module Cpu (
  Cpu,
  runCpu,
  step,

  printInstruction,
  printRegisters,
  printMemory
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Array.IO
import Data.Bits
import Data.Bits.Extra
import Data.Bits.Lens
import Data.Bool
import Data.Kind
import Data.Word
import Text.Printf

import Rom


newtype Cpu a = Cpu { unCpuM :: StateT CpuS IO a }
 deriving (Functor, Applicative, Monad, MonadIO, MonadState CpuS)

data CpuS = CpuS {
  a_   :: !Word8,
  f_   :: !Word8,
  b_   :: !Word8,
  c_   :: !Word8,
  d_   :: !Word8,
  e_   :: !Word8,
  h_   :: !Word8,
  l_   :: !Word8,
  mem_ :: IOUArray Word16 Word8,
  pc_  :: !Word16,
  sp_  :: !Word16
}

printRegisters :: Cpu ()
printRegisters = do
  cpu <- get
  liftIO $ do
    putStrLn "General Purpose Registers:"
    let rs = [("A", a_), ("B", b_), ("C", c_), ("D", d_), ("E", e_), ("H", h_),
              ("L", l_)]
    mapM_ (\(n, fn) -> printf "%2s: 0x%02x\n" n . fn $ cpu) rs
    putStrLn ""

    putStrLn "Flags:"
    let fs = [("Z", 7), ("N", 6), ("H", 5), ("C", 4)]
    forM_ fs $ \(n, bn) ->
      printf "%2s: %d\n" n . bool 0 (1 :: Int) . testBit (f_ cpu) $ bn
    putStrLn ""

    putStrLn "Special Registers:"
    let ss = [("PC", pc_), ("SP", sp_)]
    mapM_ (\(n, fn) -> printf "%2s: 0x%04x\n" n . fn $ cpu) ss

printMemory :: Cpu ()
printMemory = do
  mem <- gets mem_
  liftIO $ do
    forM_ [0, 0x0010..0xffff] $ \block -> do
      printBlock mem block
      putStrLn ""

printBlock :: IOUArray Word16 Word8 -> Word16 -> IO ()
printBlock mem block = do
  printf "%04x: " block
  forM_ [block, block+0x0002..block+0x000f] $ \group -> do
    forM_ [group, group+0x001] $
      readArray mem >=> printf "%02x"
    putStr " "

printInstruction :: Cpu ()
printInstruction = do
  mem <- gets mem_
  pc' <- gets pc_
  let block = pc' - (pc' `mod` 16)
  liftIO $ do
    printf "%2s: 0x%04x\n" "PC" pc'
    printBlock mem block
  return ()

-- Memory map
--
-- 0x0000 – 0x0100 — Nintendo boot ROM
-- 0x0100 – 0x3FFF — ROM0
--   0x0100 – 0x014F — Cartridge header
--   0x0150 – 0x3FFF — ...


runCpu :: Cpu a -> IO a
runCpu cpu = do
  s <- initCpuS
  flip evalStateT s . unCpuM $ cpu

initCpuS :: IO CpuS
initCpuS = do
  mem <- newArray_ (0, 0xffff)

  putStrLn "Loading Blargg's CPU instruction test ROM"
  loadRom "roms/blargg/cpu_instrs/cpu_instrs.gb" mem

  -- Initialise the CPU to the values set by the Nintendo boot rom.
  return CpuS {
    a_   = 0x01,
    f_   = 0xb0,
    b_   = 0x00,
    c_   = 0x13,
    d_   = 0x00,
    e_   = 0xd8,
    h_   = 0x01,
    l_   = 0x4d,
    mem_ = mem,
    pc_  = 0x0100,
    sp_  = 0xfffe
  }

class CpuStorable l where
  type CpuType l :: Type
  peek :: l -> Cpu (CpuType l)
  poke :: l -> CpuType l -> Cpu ()


pokeWith :: CpuStorable l => l -> (CpuType l -> CpuType l) -> Cpu ()
pokeWith ln fn = do
  x <- peek ln
  poke ln $ fn x

pokeWith' :: CpuStorable l => l -> (CpuType l -> CpuType l) -> Cpu (CpuType l)
pokeWith' ln fn = do
  x <- peek ln
  let x' = fn x
  poke ln x'
  return x'


-- A memory location that stores an 8-bit value.
newtype Location = Location { unLocation :: Word16 }
 deriving (Eq, Ord, Show, Num)

instance CpuStorable Location
 where
  type CpuType Location = Word8
  peek (Location addr) = do
    mem <- gets mem_
    liftIO . readArray mem $ addr
  poke (Location addr) x = do
    mem <- gets mem_
    liftIO . writeArray mem addr $ x


-- A memory location that stores a 16-bit value.
newtype Location16 = Location16 { unLocation16 :: Word16 }
 deriving (Eq, Ord, Show, Num)

instance CpuStorable Location16
 where
  type CpuType Location16 = Word16
  peek (Location16 addr) = do
    mem <- gets mem_
    lo <- liftIO . readArray mem $ addr
    hi <- liftIO . readArray mem $ addr + 1
    return . word16 hi $ lo
  poke (Location16 addr) x = do
    mem <- gets mem_
    let (hi, lo) = unWord16 x
    liftIO . writeArray mem addr $ lo
    liftIO . writeArray mem (addr + 1) $ hi


-- Registers

newtype Register = Register (Lens' CpuS Word8)

instance CpuStorable Register
 where
  type CpuType Register = Word8
  peek (Register r) = gets (^. r)
  poke (Register r) x = modify (r .~ x)


-- Accumulator
a :: Register
a = Register $ lens a_ (\s x -> s { a_ = x })

-- Flags
flens :: Lens' CpuS Word8
flens = lens f_ (\s x -> s { f_ = x })

f :: Register
f = Register flens

-- General purpose
b :: Register
b = Register $ lens b_ (\s x -> s { b_ = x })

-- General purpose
c :: Register
c = Register $ lens c_ (\s x -> s { c_ = x })

-- General purpose
d :: Register
d = Register $ lens d_ (\s x -> s { d_ = x })

-- General purpose
e :: Register
e = Register $ lens e_ (\s x -> s { e_ = x })

-- General purpose
h :: Register
h = Register $ lens h_ (\s x -> s { h_ = x })

-- General purpose
l :: Register
l = Register $ lens l_ (\s x -> s { l_ = x })


-- Flags

newtype Flag = Flag (Lens' CpuS Bool)

instance CpuStorable Flag
 where
  type CpuType Flag = Bool
  peek (Flag fl) = gets (^. fl)
  poke (Flag fl) x = modify (fl .~ x)

-- Zero flag
zf :: Flag
zf = Flag $ flens . bitAt 7

-- Subtract flag
nf :: Flag
nf = Flag $ flens . bitAt 6

-- Half carry flag
hf :: Flag
hf = Flag $ flens . bitAt 5

-- Carry flag
cf :: Flag
cf = Flag $ flens . bitAt 4

-- 16-bit virtual registers

newtype Register16 = Register16 (Lens' CpuS Word16)

instance CpuStorable Register16
 where
  type CpuType Register16 = Word16
  peek (Register16 r) = gets (^. r)
  poke (Register16 r) x = modify (r .~ x)


af :: Register16
af = register16 a f

bc :: Register16
bc = register16 c b

de :: Register16
de = register16 e d

hl :: Register16
hl = register16 l h

pc :: Register16
pc = Register16 $ lens pc_ (\s x -> s { pc_ = x })

sp :: Register16
sp = Register16 $ lens sp_ (\s x -> s { sp_ = x })


register16 :: Register -> Register -> Register16
register16 (Register hi) (Register lo) = Register16 $ lens getter setter
 where
  getter :: CpuS -> Word16
  getter cpu =
    let xhi = view hi cpu
        xlo = view lo cpu
    in word16 xhi xlo
  setter :: CpuS -> Word16 -> CpuS
  setter cpu x =
    let (xhi, xlo) = unWord16 x
    in (lo .~ xlo) . (hi .~ xhi) $ cpu


-- Executes the next instruction
--
-- TODO Move all single use instructions (those in do blocks here) to variables
-- for easier maintenance.
step :: Cpu ()
step = do
  pc' <- peek pc
  opCode <- peek $ Location pc'

  -- For op codes and descriptions see:
  -- https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
  -- https://rgbds.gbdev.io/docs/v0.9.2/gbz80.7
  case opCode of

    -- 0x0x

    -- NOP
    0x00 -> noop
    -- LD BC d16
    0x01 -> ld_r16_N16 bc
    -- LD (BC) A
    0x02 -> ld_R16_r8 bc a
    -- INC BC
    0x03 -> inc_r16 bc
    -- INC B
    0x04 -> inc_r8 b
    -- DEC B
    0x05 -> dec_r8 b
    -- LD B d8
    0x06 -> ld_r8_n8 b
    -- RLCA
    0x07 -> rotateL_a
    -- LD (a16) SP
    0x08 -> ld_N16_sp
    -- ADD HL BC
    0x09 -> add_hl_r16 bc
    -- LD A (BC)
    0x0a -> ld_a_R16 bc
    -- DEC BC
    0x0b -> dec_r16 bc
    -- INC C
    0x0c -> inc_r8 c
    -- DEC C
    0x0d -> dec_r8 c
    -- LD C d8
    0x0e -> ld_r8_n8 c
    -- RRCA
    0x0f -> rotateR_a

    -- 0x1x

    -- TODO STOP
    0x10 -> do
      -- NB: Corrupted STOP instructions hang the CPU i.e. anything other than
      -- 0x10 0x00
      incPc 2
      syncCycles 4
    -- LD DE d16
    0x11 -> ld_r16_N16 de
    -- LD (DE) A
    0x12 -> ld_R16_r8 de a
    -- INC DE
    0x13 -> inc_r16 de
    -- INC D
    0x14 -> inc_r8 d
    -- DEC D
    0x15 -> dec_r8 d
    -- LD D d8
    0x16 -> ld_r8_n8 b
    -- RLA
    0x17 -> rotateLC_a
    -- JR r8
    0x18 -> jr
    -- ADD HL DE
    0x19 -> add_hl_r16 de
    -- LD A (DE)
    0x1a -> ld_a_R16 de
    -- DEC DE
    0x1b -> dec_r16 de
    -- INC E
    0x1c -> inc_r8 e
    -- DEC E
    0x1d -> dec_r8 e
    -- LD E d8
    0x1e -> ld_r8_n8 e
    -- RRA
    0x1f -> rotateRC_a

    -- 0x2x
    -- JR NZ r8
    0x20 -> jrNCc zf
    -- LD HL d16
    0x21 -> ld_r16_N16 hl
    -- LD (HL+) A
    0x22 -> ld_HL'_a Inc
    -- INC HL
    0x23 -> inc_r16 hl
    -- INC H
    0x24 -> inc_r8 h
    -- DEC H
    0x25 -> dec_r8 h
    -- LD H d8
    0x26 -> ld_r8_n8 h
    -- DAA
    0x27 -> undefined
    -- JR Z r8
    0x28 -> jrCc zf
    -- ADD HL HL
    0x29 -> add_hl_r16 hl
    -- LD A (HL+)
    0x2a -> ld_a_HL' Inc
    -- DEC HL
    0x2b -> dec_r16 hl
    -- INC L
    0x2c -> inc_r8 l
    -- DEC L
    0x2d -> dec_r8 l
    -- LD L d8
    0x2e -> ld_r8_n8 l
    -- CPL
    0x2f -> complementA

    -- 0x3x
    -- JR NC r8
    0x30 -> jrNCc cf
    -- LD SP d16
    0x31 -> ld_r16_N16 sp
    -- LD (HL-) A
    0x32 -> ld_HL'_a Dec
    -- INC SP
    0x33 -> inc_r16 sp
    -- INC (HL)
    0x34 -> inc_HL
    -- DEC (HL)
    0x35 -> dec_HL
    -- LD (HL) d8
    0x36 -> ld_HL_n8
    -- SCF
    0x37 -> setCarry
    -- JR C r8
    0x38 -> jrCc cf
    -- ADD HL SP
    0x39 -> add_hl_r16 sp
    -- LD A (HL-)
    0x3a -> ld_a_HL' Dec
    -- DEC SP
    0x3b -> dec_r16 sp
    -- INC A
    0x3c -> inc_r8 a
    -- DEC A
    0x3d -> dec_r8 a
    -- LD A d8
    0x3e -> ld_r8_n8 a
    -- CCF
    0x3f -> complementCarry

    -- 0x4x

    -- LD B B
    0x40 -> ld_r8_r8 b b
    -- LD B C
    0x41 -> ld_r8_r8 b c
    -- LD B D
    0x42 -> ld_r8_r8 b d
    -- LD B E
    0x43 -> ld_r8_r8 b e
    -- LD B H
    0x44 -> ld_r8_r8 b h
    -- LD B L
    0x45 -> ld_r8_r8 b l
    -- LD B (HL)
    0x46 -> ld_r8_R16 b hl
    -- LD B A
    0x47 -> ld_r8_r8 b a
    -- LD C B
    0x48 -> ld_r8_r8 c b
    -- LD C C
    0x49 -> ld_r8_r8 c c
    -- LD C D
    0x4a -> ld_r8_r8 c d
    -- LD C E
    0x4b -> ld_r8_r8 c e
    -- LD C H
    0x4c -> ld_r8_r8 c h
    -- LD C L
    0x4d -> ld_r8_r8 c l
    -- LD C (HL)
    0x4e -> ld_r8_R16 c hl
    -- LD C A
    0x4f -> ld_r8_r8 c a

    -- 0x5x

    -- LD D B
    0x50 -> ld_r8_r8 d b
    -- LD D C
    0x51 -> ld_r8_r8 d c
    -- LD D D
    0x52 -> ld_r8_r8 d d
    -- LD D E
    0x53 -> ld_r8_r8 d e
    -- LD D H
    0x54 -> ld_r8_r8 d h
    -- LD D L
    0x55 -> ld_r8_r8 d l
    -- LD D (HL)
    0x56 -> ld_r8_R16 d hl
    -- LD D A
    0x57 -> ld_r8_r8 d a
    -- LD E B
    0x58 -> ld_r8_r8 e b
    -- LD E C
    0x59 -> ld_r8_r8 e c
    -- LD E D
    0x5a -> ld_r8_r8 e d
    -- LD E E
    0x5b -> ld_r8_r8 e e
    -- LD E H
    0x5c -> ld_r8_r8 e h
    -- LD E L
    0x5d -> ld_r8_r8 e l
    -- LD E (HL)
    0x5e -> ld_r8_R16 e hl
    -- LD E A
    0x5f -> ld_r8_r8 e a

    -- 0x6x

    -- LD H B
    0x60 -> ld_r8_r8 h b
    -- LD H C
    0x61 -> ld_r8_r8 h c
    -- LD H D
    0x62 -> ld_r8_r8 h d
    -- LD H E
    0x63 -> ld_r8_r8 h e
    -- LD H H
    0x64 -> ld_r8_r8 h h
    -- LD H L
    0x65 -> ld_r8_r8 h l
    -- LD H (HL)
    0x66 -> ld_r8_R16 h hl
    -- LD H A
    0x67 -> ld_r8_r8 h a
    -- LD L B
    0x68 -> ld_r8_r8 l b
    -- LD L C
    0x69 -> ld_r8_r8 l c
    -- LD L D
    0x6a -> ld_r8_r8 l d
    -- LD L E
    0x6b -> ld_r8_r8 l e
    -- LD L H
    0x6c -> ld_r8_r8 l h
    -- LD L L
    0x6d -> ld_r8_r8 l l
    -- LD L (HL)
    0x6e -> ld_r8_R16 l hl
    -- LD L A
    0x6f -> ld_r8_r8 l a

    -- 0x7x

    -- LD (HL) B
    0x70 -> ld_R16_r8 hl b
    -- LD (HL) C
    0x71 -> ld_R16_r8 hl c
    -- LD (HL) D
    0x72 -> ld_R16_r8 hl d
    -- LD (HL) E
    0x73 -> ld_R16_r8 hl e
    -- LD (HL) H
    0x74 -> ld_R16_r8 hl h
    -- LD (HL) L
    0x75 -> ld_R16_r8 hl l
    -- HALT
    0x76 -> undefined
    -- LD (HL) A
    0x77 -> ld_R16_r8 hl a
    -- LD A B
    0x78 -> ld_r8_r8 a b
    -- LD A C
    0x79 -> ld_r8_r8 a c
    -- LD A D
    0x7a -> ld_r8_r8 a d
    -- LD A E
    0x7b -> ld_r8_r8 a e
    -- LD A H
    0x7c -> ld_r8_r8 a h
    -- LD A L
    0x7d -> ld_r8_r8 a l
    -- LD A (HL)
    0x7e -> ld_r8_R16 a hl
    -- LD A A
    0x7f -> ld_r8_r8 a a

    -- 0x8x
    -- ADD A B
    0x80 -> add_r8 b
    -- ADD A C
    0x81 -> add_r8 c
    -- ADD A D
    0x82 -> add_r8 d
    -- ADD A E
    0x83 -> add_r8 e
    -- ADD A H
    0x84 -> add_r8 h
    -- ADD A L
    0x85 -> add_r8 l
    -- ADD A (HL)
    0x86 -> add_HL
    -- ADD A A
    0x87 -> add_r8 a
    -- ADC A B
    0x88 -> adc_a_r8 b
    -- ADC A C
    0x89 -> adc_a_r8 c
    -- ADC A D
    0x8a -> adc_a_r8 d
    -- ADC A E
    0x8b -> adc_a_r8 e
    -- ADC A H
    0x8c -> adc_a_r8 h
    -- ADC A L
    0x8d -> adc_a_r8 l
    -- ADC A (HL)
    0x8e -> adc_a_HL
    -- ADC A A
    0x8f -> adc_a_r8 a

    -- 0x9x
    -- SUB B
    0x90 -> sub_r8 b
    -- SUB C
    0x91 -> sub_r8 c
    -- SUB D
    0x92 -> sub_r8 d
    -- SUB E
    0x93 -> sub_r8 e
    -- SUB H
    0x94 -> sub_r8 h
    -- SUB L
    0x95 -> sub_r8 l
    -- SUB (HL)
    0x96 -> sub_HL
    -- SUB A
    0x97 -> sub_r8 a
    -- SBC A B
    0x98 -> sbc_r8 b
    -- SBC A C
    0x99 -> sbc_r8 c
    -- SBC A D
    0x9a -> sbc_r8 d
    -- SBC A E
    0x9b -> sbc_r8 e
    -- SBC A H
    0x9c -> sbc_r8 h
    -- SBC A L
    0x9d -> sbc_r8 l
    -- SBC A (HL)
    0x9e -> sbc_HL
    -- SBC A A
    0x9f -> sbc_r8 a

    -- 0xAx
    -- AND B
    0xa0 -> and_r8 b
    -- AND C
    0xa1 -> and_r8 c
    -- AND D
    0xa2 -> and_r8 d
    -- AND E
    0xa3 -> and_r8 e
    -- AND H
    0xa4 -> and_r8 h
    -- AND L
    0xa5 -> and_r8 l
    -- AND (HL)
    0xa6 -> and_HL
    -- AND A
    0xa7 -> and_r8 a
    -- XOR B
    0xa8 -> xor_r8 b
    -- XOR C
    0xa9 -> xor_r8 c
    -- XOR D
    0xaa -> xor_r8 d
    -- XOR E
    0xab -> xor_r8 e
    -- XOR H
    0xac -> xor_r8 h
    -- XOR L
    0xad -> xor_r8 l
    -- XOR (HL)
    0xae -> xor_HL
    -- XOR A
    0xaf -> xor_r8 a

    -- 0xBx
    -- OR B
    0xb0 -> or_r8 b
    -- OR C
    0xb1 -> or_r8 c
    -- OR D
    0xb2 -> or_r8 d
    -- OR E
    0xb3 -> or_r8 e
    -- OR H
    0xb4 -> or_r8 h
    -- OR L
    0xb5 -> or_r8 l
    -- OR (HL)
    0xb6 -> or_HL
    -- OR A
    0xb7 -> or_r8 a
    -- CP B
    0xb8 -> compare_r8 b
    -- CP C
    0xb9 -> compare_r8 c
    -- CP D
    0xba -> compare_r8 d
    -- CP E
    0xbb -> compare_r8 e
    -- CP H
    0xbc -> compare_r8 h
    -- CP L
    0xbd -> compare_r8 l
    -- CP (HL)
    0xbe -> compare_HL
    -- CP A
    0xbf -> compare_r8 a

    -- 0xCx
    0xc0 -> undefined
    0xc1 -> undefined
    0xc2 -> undefined
    0xc3 -> undefined
    0xc4 -> undefined
    0xc5 -> undefined
    0xc6 -> undefined
    0xc7 -> undefined
    0xc8 -> undefined
    0xc9 -> undefined
    0xca -> undefined
    -- PREFIX CB
    0xcb -> prefixCB
    0xcc -> undefined
    0xcd -> undefined
    0xce -> undefined
    0xcf -> undefined

    -- 0xDx
    0xd0 -> undefined
    0xd1 -> undefined
    0xd2 -> undefined
    -- ILLEGAL
    0xd3 -> undefined
    0xd4 -> undefined
    0xd5 -> undefined
    0xd6 -> undefined
    0xd7 -> undefined
    0xd8 -> undefined
    0xd9 -> undefined
    0xda -> undefined
    -- ILLEGAL
    0xdb -> undefined
    0xdc -> undefined
    -- ILLEGAL
    0xdd -> undefined
    0xde -> undefined
    0xdf -> undefined

    -- 0xEx
    0xe0 -> undefined
    0xe1 -> undefined
    0xe2 -> undefined
    -- ILLEGAL
    0xe3 -> undefined
    -- ILLEGAL
    0xe4 -> undefined
    0xe5 -> undefined
    0xe6 -> undefined
    0xe7 -> undefined
    0xe8 -> undefined
    0xe9 -> undefined
    0xea -> undefined
    -- ILLEGAL
    0xeb -> undefined
    -- ILLEGAL
    0xec -> undefined
    -- ILLEGAL
    0xed -> undefined
    0xee -> undefined
    0xef -> undefined

    -- 0xFx
    0xf0 -> undefined
    0xf1 -> undefined
    0xf2 -> undefined
    0xf3 -> undefined
    -- ILLEGAL
    0xf4 -> undefined
    0xf5 -> undefined
    0xf6 -> undefined
    0xf7 -> undefined
    0xf8 -> undefined
    0xf9 -> undefined
    0xfa -> undefined
    0xfb -> undefined
    -- ILLEGAL
    0xfc -> undefined
    -- ILLEGAL
    0xfd -> undefined
    0xfe -> undefined
    0xff -> undefined

    -- Unreachable, keep the linter happer
    _    -> undefined


prefixCB :: Cpu ()
prefixCB = do
  pc' <- peek pc
  opCode <- peek $ Location pc'
  -- For op codes and descriptions see:
  -- https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
  -- https://rgbds.gbdev.io/docs/v0.9.2/gbz80.7
  case opCode of

    -- 0xCB 0x0x
    -- RLC B
    0x00 -> rotateL_r8 b
    -- RLC C
    0x01 -> rotateL_r8 c
    -- RLC D
    0x02 -> rotateL_r8 d
    -- RLC E
    0x03 -> rotateL_r8 e
    -- RLC H
    0x04 -> rotateL_r8 h
    -- RLC L
    0x05 -> rotateL_r8 l
    -- RLC (HL)
    0x06 -> rotateL_HL
    -- RLC A
    0x07 -> rotateL_r8 a
    -- RRC B
    0x08 -> rotateR_r8 b
    -- RRC C
    0x09 -> rotateR_r8 c
    -- RRC D
    0x0a -> rotateR_r8 d
    -- RRC E
    0x0b -> rotateR_r8 e
    -- RRC H
    0x0c -> rotateR_r8 h
    -- RRC L
    0x0d -> rotateR_r8 l
    -- RRC (HL)
    0x0e -> rotateR_HL
    -- RRC A
    0x0f -> rotateR_r8 a

    -- 0xCB 0x1x
    -- RL B
    0x10 -> rotateLC_r8 b
    -- RL C
    0x11 -> rotateLC_r8 c
    -- RL D
    0x12 -> rotateLC_r8 d
    -- RL E
    0x13 -> rotateLC_r8 e
    -- RL H
    0x14 -> rotateLC_r8 h
    -- RL L
    0x15 -> rotateLC_r8 l
    -- RL (HL)
    0x16 -> rotateLC_HL
    -- RL A
    0x17 -> rotateLC_r8 a
    -- RR B
    0x18 -> rotateRC_r8 b
    -- RR C
    0x19 -> rotateRC_r8 c
    -- RR D
    0x1a -> rotateRC_r8 d
    -- RR E
    0x1b -> rotateRC_r8 e
    -- RR H
    0x1c -> rotateRC_r8 h
    -- RR L
    0x1d -> rotateRC_r8 l
    -- RR (HL)
    0x1e -> rotateRC_HL
    -- RR A
    0x1f -> rotateRC_r8 a

    -- 0xCB 0x2x
    -- SLA B
    0x20 -> shiftLeftArithmetically_r8 b
    -- SLA C
    0x21 -> shiftLeftArithmetically_r8 c
    -- SLA D
    0x22 -> shiftLeftArithmetically_r8 d
    -- SLA E
    0x23 -> shiftLeftArithmetically_r8 e
    -- SLA H
    0x24 -> shiftLeftArithmetically_r8 h
    -- SLA L
    0x25 -> shiftLeftArithmetically_r8 l
    -- SLA (HL)
    0x26 -> shiftLeftArithmetically_HL
    -- SLA A
    0x27 -> shiftLeftArithmetically_r8 a
    -- SRA B
    0x28 -> shiftRightArithmetically_r8 b
    -- SRA C
    0x29 -> shiftRightArithmetically_r8 c
    -- SRA D
    0x2a -> shiftRightArithmetically_r8 d
    -- SRA E
    0x2b -> shiftRightArithmetically_r8 e
    -- SRA H
    0x2c -> shiftRightArithmetically_r8 h
    -- SRA L
    0x2d -> shiftRightArithmetically_r8 l
    -- SRA (HL)
    0x2e -> shiftRightArithmetically_HL
    -- SRA A
    0x2f -> shiftRightArithmetically_r8 a

    -- 0xCB 0x3x
    -- SWAP B
    0x30 -> swap_r8 b
    -- SWAP C
    0x31 -> swap_r8 c
    -- SWAP D
    0x32 -> swap_r8 d
    -- SWAP E
    0x33 -> swap_r8 e
    -- SWAP H
    0x34 -> swap_r8 h
    -- SWAP L
    0x35 -> swap_r8 l
    -- SWAP (HL)
    0x36 -> swap_HL
    -- SWAP A
    0x37 -> swap_r8 a
    -- SRL B
    0x38 -> shiftRightLogical_r8 b
    -- SRL C
    0x39 -> shiftRightLogical_r8 c
    -- SRL D
    0x3a -> shiftRightLogical_r8 d
    -- SRL E
    0x3b -> shiftRightLogical_r8 e
    -- SRL H
    0x3c -> shiftRightLogical_r8 h
    -- SRL L
    0x3d -> shiftRightLogical_r8 l
    -- SRL (HL)
    0x3e -> shiftRightLogical_HL
    -- SRL A
    0x3f -> shiftRightLogical_r8 a

    -- 0xCB 0x4x
    -- BIT 0 B
    0x40 -> test_b_r8 0 b
    -- BIT 0 C
    0x41 -> test_b_r8 0 c
    -- BIT 0 D
    0x42 -> test_b_r8 0 d
    -- BIT 0 E
    0x43 -> test_b_r8 0 e
    -- BIT 0 H
    0x44 -> test_b_r8 0 h
    -- BIT 0 L
    0x45 -> test_b_r8 0 l
    -- BIT 0 (HL)
    0x46 -> test_b_HL 0
    -- BIT 0 A
    0x47 -> test_b_r8 0 a
    -- BIT 1 B
    0x48 -> test_b_r8 1 b
    -- BIT 1 C
    0x49 -> test_b_r8 1 c
    -- BIT 1 D
    0x4a -> test_b_r8 1 d
    -- BIT 1 E
    0x4b -> test_b_r8 1 e
    -- BIT 1 H
    0x4c -> test_b_r8 1 h
    -- BIT 1 L
    0x4d -> test_b_r8 1 l
    -- BIT 1 (HL)
    0x4e -> test_b_HL 1
    -- BIT 1 A
    0x4f -> test_b_r8 1 a

    -- 0xCB 0x5x
    -- BIT 2 B
    0x50 -> test_b_r8 2 b
    -- BIT 2 C
    0x51 -> test_b_r8 2 c
    -- BIT 2 D
    0x52 -> test_b_r8 2 d
    -- BIT 2 E
    0x53 -> test_b_r8 2 e
    -- BIT 2 H
    0x54 -> test_b_r8 2 h
    -- BIT 2 L
    0x55 -> test_b_r8 2 l
    -- BIT 2 (HL)
    0x56 -> test_b_HL 2
    -- BIT 2 A
    0x57 -> test_b_r8 2 a
    -- BIT 3 B
    0x58 -> test_b_r8 3 b
    -- BIT 3 C
    0x59 -> test_b_r8 3 c
    -- BIT 3 D
    0x5a -> test_b_r8 3 d
    -- BIT 3 E
    0x5b -> test_b_r8 3 e
    -- BIT 3 H
    0x5c -> test_b_r8 3 h
    -- BIT 3 L
    0x5d -> test_b_r8 3 l
    -- BIT 3 (HL)
    0x5e -> test_b_HL 3
    -- BIT 3 A
    0x5f -> test_b_r8 3 a

    -- 0xCB 0x5x
    -- BIT 4 B
    0x60 -> test_b_r8 4 b
    -- BIT 4 C
    0x61 -> test_b_r8 4 c
    -- BIT 4 D
    0x62 -> test_b_r8 4 d
    -- BIT 4 E
    0x63 -> test_b_r8 4 e
    -- BIT 4 H
    0x64 -> test_b_r8 4 h
    -- BIT 4 L
    0x65 -> test_b_r8 4 l
    -- BIT 4 (HL)
    0x66 -> test_b_HL 4
    -- BIT 4 A
    0x67 -> test_b_r8 4 a
    -- BIT 5 B
    0x68 -> test_b_r8 5 b
    -- BIT 5 C
    0x69 -> test_b_r8 5 c
    -- BIT 5 D
    0x6a -> test_b_r8 5 d
    -- BIT 5 E
    0x6b -> test_b_r8 5 e
    -- BIT 5 H
    0x6c -> test_b_r8 5 h
    -- BIT 5 L
    0x6d -> test_b_r8 5 l
    -- BIT 5 (HL)
    0x6e -> test_b_HL 5
    -- BIT 5 A
    0x6f -> test_b_r8 5 a

    -- 0xCB 0xBx
    -- BIT 6 B
    0x70 -> test_b_r8 6 b
    -- BIT 6 C
    0x71 -> test_b_r8 6 c
    -- BIT 6 D
    0x72 -> test_b_r8 6 d
    -- BIT 6 E
    0x73 -> test_b_r8 6 e
    -- BIT 6 H
    0x74 -> test_b_r8 6 h
    -- BIT 6 L
    0x75 -> test_b_r8 6 l
    -- BIT 6 (HL)
    0x76 -> test_b_HL 6
    -- BIT 6 A
    0x77 -> test_b_r8 6 a
    -- BIT 7 B
    0x78 -> test_b_r8 7 b
    -- BIT 7 C
    0x79 -> test_b_r8 7 c
    -- BIT 7 D
    0x7a -> test_b_r8 7 d
    -- BIT 7 E
    0x7b -> test_b_r8 7 e
    -- BIT 7 H
    0x7c -> test_b_r8 7 h
    -- BIT 7 L
    0x7d -> test_b_r8 7 l
    -- BIT 7 (HL)
    0x7e -> test_b_HL 7
    -- BIT 7 A
    0x7f -> test_b_r8 7 a

    -- 0xCB 0x8x
    -- RES 0 B
    0x80 -> reset_b_r8 0 b
    -- RES 0 C
    0x81 -> reset_b_r8 0 c
    -- RES 0 D
    0x82 -> reset_b_r8 0 d
    -- RES 0 E
    0x83 -> reset_b_r8 0 e
    -- RES 0 H
    0x84 -> reset_b_r8 0 h
    -- RES 0 L
    0x85 -> reset_b_r8 0 l
    -- RES 0 (HL)
    0x86 -> reset_b_HL 0
    -- RES 0 A
    0x87 -> reset_b_r8 0 a
    -- RES 1 B
    0x88 -> reset_b_r8 1 b
    -- RES 1 C
    0x89 -> reset_b_r8 1 c
    -- RES 1 D
    0x8a -> reset_b_r8 1 d
    -- RES 1 E
    0x8b -> reset_b_r8 1 e
    -- RES 1 H
    0x8c -> reset_b_r8 1 h
    -- RES 1 L
    0x8d -> reset_b_r8 1 l
    -- RES 1 (HL)
    0x8e -> reset_b_HL 1
    -- RES 1 A
    0x8f -> reset_b_r8 1 a

    -- 0xCB 0x9x
    -- RES 2 B
    0x90 -> reset_b_r8 2 b
    -- RES 2 C
    0x91 -> reset_b_r8 2 c
    -- RES 2 D
    0x92 -> reset_b_r8 2 d
    -- RES 2 E
    0x93 -> reset_b_r8 2 e
    -- RES 2 H
    0x94 -> reset_b_r8 2 h
    -- RES 2 L
    0x95 -> reset_b_r8 2 l
    -- RES 2 (HL)
    0x96 -> reset_b_HL 2
    -- RES 2 A
    0x97 -> reset_b_r8 2 a
    -- RES 3 B
    0x98 -> reset_b_r8 3 b
    -- RES 3 C
    0x99 -> reset_b_r8 3 c
    -- RES 3 D
    0x9a -> reset_b_r8 3 d
    -- RES 3 E
    0x9b -> reset_b_r8 3 e
    -- RES 3 H
    0x9c -> reset_b_r8 3 h
    -- RES 3 L
    0x9d -> reset_b_r8 3 l
    -- RES 3 (HL)
    0x9e -> reset_b_HL 3
    -- RES 3 A
    0x9f -> reset_b_r8 3 a

    -- 0xCB 0xAx
    -- RES 4 B
    0xa0 -> reset_b_r8 4 b
    -- RES 4 C
    0xa1 -> reset_b_r8 4 c
    -- RES 4 D
    0xa2 -> reset_b_r8 4 d
    -- RES 4 E
    0xa3 -> reset_b_r8 4 e
    -- RES 4 H
    0xa4 -> reset_b_r8 4 h
    -- RES 4 L
    0xa5 -> reset_b_r8 4 l
    -- RES 4 (HL)
    0xa6 -> reset_b_HL 4
    -- RES 4 A
    0xa7 -> reset_b_r8 4 a
    -- RES 5 B
    0xa8 -> reset_b_r8 5 b
    -- RES 5 C
    0xa9 -> reset_b_r8 5 c
    -- RES 5 D
    0xaa -> reset_b_r8 5 d
    -- RES 5 E
    0xab -> reset_b_r8 5 e
    -- RES 5 H
    0xac -> reset_b_r8 5 h
    -- RES 5 L
    0xad -> reset_b_r8 5 l
    -- RES 5 (HL)
    0xae -> reset_b_HL 5
    -- RES 5 A
    0xaf -> reset_b_r8 5 a

    -- 0xCB 0xBx
    -- RES 6 B
    0xb0 -> reset_b_r8 6 b
    -- RES 6 C
    0xb1 -> reset_b_r8 6 c
    -- RES 6 D
    0xb2 -> reset_b_r8 6 d
    -- RES 6 E
    0xb3 -> reset_b_r8 6 e
    -- RES 6 H
    0xb4 -> reset_b_r8 6 h
    -- RES 6 L
    0xb5 -> reset_b_r8 6 l
    -- RES 6 (HL)
    0xb6 -> reset_b_HL 6
    -- RES 6 A
    0xb7 -> reset_b_r8 6 a
    -- RES 7 B
    0xb8 -> reset_b_r8 7 b
    -- RES 7 C
    0xb9 -> reset_b_r8 7 c
    -- RES 7 D
    0xba -> reset_b_r8 7 d
    -- RES 7 E
    0xbb -> reset_b_r8 7 e
    -- RES 7 H
    0xbc -> reset_b_r8 7 h
    -- RES 7 L
    0xbd -> reset_b_r8 7 l
    -- RES 7 (HL)
    0xbe -> reset_b_HL 7
    -- RES 7 A
    0xbf -> reset_b_r8 7 a

    -- 0xCB 0xCx
    -- SET 0 B
    0xc0 -> set_b_r8 0 b
    -- SET 0 C
    0xc1 -> set_b_r8 0 c
    -- SET 0 D
    0xc2 -> set_b_r8 0 d
    -- SET 0 E
    0xc3 -> set_b_r8 0 e
    -- SET 0 H
    0xc4 -> set_b_r8 0 h
    -- SET 0 L
    0xc5 -> set_b_r8 0 l
    -- SET 0 (HL)
    0xc6 -> set_b_HL 0
    -- SET 0 A
    0xc7 -> set_b_r8 0 a
    -- SET 1 B
    0xc8 -> set_b_r8 1 b
    -- SET 1 C
    0xc9 -> set_b_r8 1 c
    -- SET 1 D
    0xca -> set_b_r8 1 d
    -- SET 1 E
    0xcb -> set_b_r8 1 e
    -- SET 1 H
    0xcc -> set_b_r8 1 h
    -- SET 1 L
    0xcd -> set_b_r8 1 l
    -- SET 1 (HL)
    0xce -> set_b_HL 1
    -- SET 1 A
    0xcf -> set_b_r8 1 a

    -- 0xCB 0xDx
    -- SET 2 B
    0xd0 -> set_b_r8 2 b
    -- SET 2 C
    0xd1 -> set_b_r8 2 c
    -- SET 2 D
    0xd2 -> set_b_r8 2 d
    -- SET 2 E
    0xd3 -> set_b_r8 2 e
    -- SET 2 H
    0xd4 -> set_b_r8 2 h
    -- SET 2 L
    0xd5 -> set_b_r8 2 l
    -- SET 2 (HL)
    0xd6 -> set_b_HL 2
    -- SET 2 A
    0xd7 -> set_b_r8 2 a
    -- SET 3 B
    0xd8 -> set_b_r8 3 b
    -- SET 3 C
    0xd9 -> set_b_r8 3 c
    -- SET 3 D
    0xda -> set_b_r8 3 d
    -- SET 3 E
    0xdb -> set_b_r8 3 e
    -- SET 3 H
    0xdc -> set_b_r8 3 h
    -- SET 3 L
    0xdd -> set_b_r8 3 l
    -- SET 3 (HL)
    0xde -> set_b_HL 3
    -- SET 3 A
    0xdf -> set_b_r8 3 a

    -- 0xCB 0xEx
    -- SET 4 B
    0xe0 -> set_b_r8 4 b
    -- SET 4 C
    0xe1 -> set_b_r8 4 c
    -- SET 4 D
    0xe2 -> set_b_r8 4 d
    -- SET 4 E
    0xe3 -> set_b_r8 4 e
    -- SET 4 H
    0xe4 -> set_b_r8 4 h
    -- SET 4 L
    0xe5 -> set_b_r8 4 l
    -- SET 4 (HL)
    0xe6 -> set_b_HL 4
    -- SET 4 A
    0xe7 -> set_b_r8 4 a
    -- SET 5 B
    0xe8 -> set_b_r8 5 b
    -- SET 5 C
    0xe9 -> set_b_r8 5 c
    -- SET 5 D
    0xea -> set_b_r8 5 d
    -- SET 5 E
    0xeb -> set_b_r8 5 e
    -- SET 5 H
    0xec -> set_b_r8 5 h
    -- SET 5 L
    0xed -> set_b_r8 5 l
    -- SET 5 (HL)
    0xee -> set_b_HL 5
    -- SET 5 A
    0xef -> set_b_r8 5 a

    -- 0xCB 0xFx
    -- SET 6 B
    0xf0 -> set_b_r8 6 b
    -- SET 6 C
    0xf1 -> set_b_r8 6 c
    -- SET 6 D
    0xf2 -> set_b_r8 6 d
    -- SET 6 E
    0xf3 -> set_b_r8 6 e
    -- SET 6 H
    0xf4 -> set_b_r8 6 h
    -- SET 6 L
    0xf5 -> set_b_r8 6 l
    -- SET 6 (HL)
    0xf6 -> set_b_HL 6
    -- SET 6 A
    0xf7 -> set_b_r8 6 a
    -- SET 7 B
    0xf8 -> set_b_r8 7 b
    -- SET 7 C
    0xf9 -> set_b_r8 7 c
    -- SET 7 D
    0xfa -> set_b_r8 7 d
    -- SET 7 E
    0xfb -> set_b_r8 7 e
    -- SET 7 H
    0xfc -> set_b_r8 7 h
    -- SET 7 L
    0xfd -> set_b_r8 7 l
    -- SET 7 (HL)
    0xfe -> set_b_HL 7
    -- SET 7 A
    0xff -> set_b_r8 7 a

    -- Unreachable, keep the linter happer
    _ -> undefined

incPc :: Word16 -> Cpu ()
incPc x = pokeWith pc (+ x)

-- Sync execution to the number of clock cycles given by n.
syncCycles :: Int -> Cpu ()
syncCycles n = return ()


-- CPU instructions
--
-- Each function here represents one or more CPU instructions that have the
-- same format, i.e. are the same size in memory and take the same number of
-- clock cycles to execute.
--
-- Format: e.g. ld_[dst]_[src]
--
--   where src/dst describes the source/destination (first/second) operand:
--
--     a|sp etc.  named register
--     r8         8-bit register
--     r16        16-bit reigster
--     n8         immediate 8-bit value
--     n16        immediate 16-bit value
--
--   when capitalised the src or dest is some kind of memory address
--
--     R16        immediate absolute address
--     HL         absolute address stored in register HL
--     HL'        "        "       "      "  "        "
--                which is incremented or decremented afterwards
--

-- Increment/decrement flag used for some instructions
data IncDec = Inc | Dec

incDec :: Num a => IncDec -> a -> a
incDec Dec = subtract 1
incDec Inc = (+ 1)

-- Arithmetic/logic instructions
--
-- If one of these instructions has a destination (everything but compare) it
-- is always understood to be the accumulator, `a'.

add_r8 :: Register -> Cpu ()
add_r8 r = do
  a' <- peek a
  r' <- peek r
  let x = a' + r'
  poke a x
  poke zf $ x == 0
  poke nf False
  poke hf $ (a' .&. 0xf + r' .&. 0xf) .&. 0x10 /= 0
  poke cf $ x < r'
  incPc 1
  syncCycles 4

add_HL :: Cpu ()
add_HL = do
  addr <- Location <$> peek hl
  x <- peek addr
  a' <- peek a
  let a'' = a' + x
  poke a a''
  poke zf $ a'' == 0
  poke nf False
  poke hf $ (a' .&. 0xf + x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' < x
  incPc 1
  syncCycles 4

add_hl_r16 :: Register16 -> Cpu ()
add_hl_r16 r = do
  hl' <- peek hl
  r' <- peek r
  let x = hl' + r'
  poke hl x
  poke nf False
  poke hf $ (hl' .&. 0xf + r' .&. 0xf) .&. 0x10 /= 0
  poke cf $ x < r'
  incPc 1
  syncCycles 8

adc_a_r8 :: Register -> Cpu ()
adc_a_r8 r = do
  c' <- bool 0 1 <$> peek cf
  a' <- peek a
  r' <- (+ c') <$> peek r
  let x = a' + r'
  poke a x
  poke zf $ x == 0x00
  poke nf False
  poke hf $ (a' .&. 0xf + r' .&. 0xf) .&. 0x10 /= 0x00
  poke cf $ x < r'
  incPc 1
  syncCycles 4

adc_a_HL :: Cpu ()
adc_a_HL = do
  c' <- bool 0 1 <$> peek cf
  a' <- peek a
  addr <- Location <$> peek hl
  x <- (+ c') <$> peek addr
  let a'' = a' + x
  poke a a''
  poke zf $ a'' == 0x00
  poke nf False
  poke hf $ (a' .&. 0xf + x .&. 0xf) .&. 0x10 /= 0x00
  poke cf $ a'' < x
  incPc 1
  syncCycles 8

and_r8 :: Register -> Cpu ()
and_r8 r = do
  a' <- (.&.) <$> peek a <*> peek r
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf True
  poke cf False
  incPc 1
  syncCycles 4

and_HL :: Cpu ()
and_HL = do
  addr <- Location <$> peek hl
  a' <- (.&.) <$> peek a <*> peek addr
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf True
  poke cf False
  incPc 1
  syncCycles 8

compare_r8 :: Register -> Cpu ()
compare_r8 r = do
  a' <- peek a
  x <- peek r
  let a'' = a' - x
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'
  incPc 1
  syncCycles 4
  return ()

compare_HL :: Cpu ()
compare_HL = do
  addr <- Location <$> peek hl
  a' <- peek a
  x <- peek addr
  let a'' = a' - x
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'
  incPc 1
  syncCycles 8
  return ()

complementA :: Cpu ()
complementA = do
  pokeWith a complement
  poke nf True
  poke hf True

or_r8 :: Register -> Cpu ()
or_r8 r = do
  a' <- (.|.) <$> peek a <*> peek r
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf False
  incPc 1
  syncCycles 4

or_HL :: Cpu ()
or_HL = do
  addr <- Location <$> peek hl
  a' <- (.|.) <$> peek a <*> peek addr
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf False
  incPc 1
  syncCycles 8

xor_r8 :: Register -> Cpu ()
xor_r8 r = do
  a' <- xor <$> peek a <*> peek r
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf False
  incPc 1
  syncCycles 4

xor_HL :: Cpu ()
xor_HL = do
  addr <- Location <$> peek hl
  a' <- xor <$> peek a <*> peek addr
  poke a a'
  poke zf $ a' == 0
  poke nf False
  poke hf False
  poke cf False
  incPc 1
  syncCycles 8

sbc_r8 :: Register -> Cpu ()
sbc_r8 r = do
  c' <- bool 0 1 <$> peek cf
  a' <- peek a
  x <- (+ c') <$> peek r
  let a'' = a' - x
  poke a a''
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'
  incPc 1
  syncCycles 4
  return ()

sbc_HL :: Cpu ()
sbc_HL = do
  addr <- Location <$> peek hl
  c' <- bool 0 1 <$> peek cf
  a' <- peek a
  x <- (+ c') <$> peek addr
  let a'' = a' - x
  poke a a''
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'
  incPc 1
  syncCycles 8
  return ()

sub_r8 :: Register -> Cpu ()
sub_r8 r = do
  a' <- peek a
  r' <- peek r
  let x = a' - r'
  poke a x
  poke zf $ x == 0
  poke nf True
  poke hf $ (a' .&. 0xf - r' .&. 0xf) .&. 0x10 /= 0
  poke cf $ x > a'
  incPc 1
  syncCycles 4
  return ()

sub_HL :: Cpu ()
sub_HL = do
  addr <- Location <$> peek hl
  a' <- peek a
  x <- peek addr
  let a'' = a' - x
  poke a a''
  poke zf $ a'' == 0
  poke nf True
  poke hf $ (a' .&. 0xf - x .&. 0xf) .&. 0x10 /= 0
  poke cf $ a'' > a'
  incPc 1
  syncCycles 8
  return ()


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


-- Load instructions

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

ld_r16_N16 :: Register16 -> Cpu ()
ld_r16_N16 dst = do
  addr <- Location16 . (+1) <$> peek pc
  x <- peek addr
  poke dst x
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
  addr <- Location16 . (+1) <$> peek pc
  x <- peek sp
  poke addr x
  incPc 3
  syncCycles 20

-- Relative jumps

jr :: Cpu ()
jr =  do
  addr <- Location16 . (+1) <$> peek pc
  x <- peek addr
  pokeWith pc (+ x)
  syncCycles 12

jrCc :: Flag -> Cpu ()
jrCc cc = do
  cc' <- peek cc
  if cc'
    then do
      addr <- Location16 . (+1) <$> peek pc
      x <- peek addr
      pokeWith pc (+ x)
      syncCycles 12
    else do
      incPc 2
      syncCycles 8

jrNCc :: Flag -> Cpu ()
jrNCc cc = do
  cc' <- not <$> peek cc
  if cc'
    then do
      addr <- Location16 . (+1) <$> peek pc
      x <- peek addr
      pokeWith pc (+ x)
      syncCycles 12
    else do
      incPc 2
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

complementCarry :: Cpu ()
complementCarry = do
  poke nf False
  poke hf False
  pokeWith cf not
  incPc 1
  syncCycles 4

setCarry :: Cpu ()
setCarry = do
  poke nf False
  poke hf False
  poke cf True
  incPc 1
  syncCycles 4

-- Misc instructions

noop :: Cpu ()
noop = do
  incPc 1
  syncCycles 4

-- Utilities

word16 :: Word8 -> Word8 -> Word16
word16 hi lo =
  let x = flip shift 8 . fromIntegral $ hi
  in x + fromIntegral lo

unWord16 :: Word16 -> (Word8, Word8)
unWord16 x =
  let hi = fromIntegral . flip shift (-8) $ x
      lo = fromIntegral x
  in (hi, lo)

swapNibbles :: Word8 -> Word8
swapNibbles =
  (.&.) <$> (`shift` (-4)) . (0xf0 .&.)
        <*> (`shift` 4) . (0x0f .&.)

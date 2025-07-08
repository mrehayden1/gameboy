module Cpu.Core (
  module Data.Array.IO,

  -- CPU monad
  Cpu(..),
  runCpu,
  evalCpu,
  execCpu,

  -- CPU state
  CpuS(..),
  initCpuS,

  -- CPU Helpers
  incPc,
  syncCycles,

  -- Storage
  CpuStorable(..),
  pokeWith,
  pokeWith',

  -- Memory
  Location(..),
  LocationInt8(..),
  Location16(..),

  -- Registers / flags
  Register,
  a,
  b,
  c,
  d,
  e,
  h,
  l,

  Register16,
  af,
  bc,
  de,
  hl,

  af',
  bc',
  de',
  hl',

  pc,
  sp,

  Flag,
  f,
  zf,
  nf,
  hf,
  cf,
  ime,

  -- Misc
  IncDec(..),
  incDec,
) where

import Control.Lens
import Control.Monad.State
import Data.Array.IO
import Data.Bits.Lens
import Data.Int
import Data.Kind
import Data.Word
import Data.Word.Extra

newtype Cpu a = Cpu { unCpu :: StateT CpuS IO a }
 deriving (Functor, Applicative, Monad, MonadIO, MonadState CpuS)

runCpu :: Cpu a -> CpuS -> IO (a, CpuS)
runCpu m s = flip runStateT s . unCpu $ m

evalCpu :: Cpu a -> CpuS -> IO a
evalCpu m s = flip evalStateT s . unCpu $ m

execCpu :: Cpu a -> CpuS -> IO CpuS
execCpu m s = flip execStateT s . unCpu $ m

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
  sp_  :: !Word16,
  ime_ :: !(Either () Bool)
}

initCpuS :: IO CpuS
initCpuS = do
  mem <- newArray_ (0, 0xffff)

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
    sp_  = 0xfffe,
    ime_ = Right False
  }


incPc :: Word16 -> Cpu ()
incPc x = pokeWith pc (+ x)

-- Sync execution to the number of clock cycles given by n.
syncCycles :: Int -> Cpu ()
syncCycles _ = return ()


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


-- A memory location that stores an unsigned 8-bit number.
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


-- A memory location that stores a signed 8-bt number.
newtype LocationInt8 = LocationInt8 { unLocationInt8 :: Word16 }

instance CpuStorable LocationInt8
 where
   type CpuType LocationInt8 = Int8
   peek (LocationInt8 addr) = do
     mem <- gets mem_
     liftIO . fmap fromIntegral . readArray mem $ addr
   poke (LocationInt8 addr) x = do
     mem <- gets mem_
     liftIO . writeArray mem addr $ fromIntegral x


-- A memory location that stores a unsigned 16-bit number.
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
f' :: Lens' CpuS Word8
f' = lens f_ (\s x -> s { f_ = x })

f :: Register
f = Register f'

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
zf = Flag $ f' . bitAt 7

-- Subtract flag
nf :: Flag
nf = Flag $ f' . bitAt 6

-- Half carry flag
hf :: Flag
hf = Flag $ f' . bitAt 5

-- Carry flag
cf :: Flag
cf = Flag $ f' . bitAt 4

-- 16-bit virtual registers

newtype Register16 = Register16 (Lens' CpuS Word16)

instance CpuStorable Register16
 where
  type CpuType Register16 = Word16
  peek (Register16 r) = gets (^. r)
  poke (Register16 r) x = modify (r .~ x)


af' :: Lens' CpuS Word16
af' = register16 a f

af :: Register16
af = Register16 af'

bc' :: Lens' CpuS Word16
bc' = register16 c b

bc :: Register16
bc = Register16 bc'

de' :: Lens' CpuS Word16
de' = register16 e d

de :: Register16
de = Register16 de'

hl' :: Lens' CpuS Word16
hl' = register16 l h

hl :: Register16
hl = Register16 hl'


pc :: Register16
pc = Register16 $ lens pc_ (\s x -> s { pc_ = x })

sp :: Register16
sp = Register16 $ lens sp_ (\s x -> s { sp_ = x })


register16 :: Register -> Register -> Lens' CpuS Word16
register16 (Register hi) (Register lo) = lens getter setter
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

-- Misc

-- Increment/decrement flag used for some instructions, isomorphic to Bool, but
-- more explicit.
data IncDec = Inc | Dec

incDec :: Num a => IncDec -> a -> a
incDec Dec = subtract 1
incDec Inc = (+ 1)


-- Interrupt master enable (IME) flag

-- Left () means the Ime flag is going to be set next instruction, but it's
-- still False.
type ImeFlag = Lens' CpuS (Either () Bool)

ime :: ImeFlag
ime = lens ime_ (\s x -> s { ime_ = x })

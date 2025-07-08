module Debugger.Instructions (
  widget
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Int
import Data.Word
import qualified Graphics.Vty as V
import Text.Printf
import Reflex
import Reflex.Network
import Reflex.Vty

import Cpu.Core
import Debugger.Widget

widget :: forall t m. Widget t m => CpuS -> Dynamic t CpuS -> m ()
widget cpu0 cpu = do
  t <- theme
  pb <- getPostBuild

  col $ do
    grout (fixed 1) blank
    row $ do
      grout (fixed 2) blank
      grout flex . void . networkHold blank <=< performEvent
        $ fmap (\t' s -> liftIO . flip evalStateT s . unCpu . decodeOp $ t') t
            <@> leftmost [cpu0 <$ pb, updated cpu]
      grout (fixed 1) blank

decodeOp :: forall t m. Widget t m => V.Attr -> Cpu (m ())
decodeOp attr = do
  pc' <- Location <$> peek pc
  opCode <- peek pc'

  tellImages . pure . pure <$> case opCode of
    -- 0x0x
    0x00 -> inst0 "NOOP"
    0x01 -> inst1 "LD BC" argU16
    0x02 -> inst0 "LD (BC) A"
    0x03 -> inst0 "INC BC"
    0x04 -> inst0 "INC B"
    0x05 -> inst0 "DEC B"
    0x06 -> inst1 "LD B" argU8
    0x07 -> inst0 "RLCA"
    0x08 -> inst1' "LD (" argU16 ") SP"
    0x09 -> inst0 "ADD HL BC"
    0x0a -> inst0 "LD A (BC)"
    0x0b -> inst0 "DEC BC"
    0x0c -> inst0 "INC C"
    0x0d -> inst0 "DEC C"
    0x0e -> inst1 "LD C" argU8
    0x0f -> inst0 "RRCA"

    -- 0x1x
    0x10 -> inst0 "STOP"
    0x11 -> inst1 "LD DE" argU16
    0x12 -> inst0 "LD (DE) A"
    0x13 -> inst0 "INC DE"
    0x14 -> inst0 "INC D"
    0x15 -> inst0 "DEC D"
    0x16 -> inst1 "LD D" argU8
    0x17 -> inst0 "RLA"
    0x18 -> inst1 "JR" argU8
    0x19 -> inst0 "ADD HL DE"
    0x1a -> inst0 "LD A (DE)"
    0x1b -> inst0 "DEC DE"
    0x1c -> inst0 "INC E"
    0x1d -> inst0 "DEC E"
    0x1e -> inst1 "LD E" argU8
    0x1f -> inst0 "RRA"

    -- 0x2x
    0x20 -> inst1 "JR NZ" argR8
    0x21 -> inst1 "LD HL" argU16
    0x22 -> inst0 "LD (HL+) A"
    0x23 -> inst0 "INC HL"
    0x24 -> inst0 "INC H"
    0x25 -> inst0 "DEC H"
    0x26 -> inst1 "LD H" argU8
    0x27 -> inst0 "DAA"
    0x28 -> inst1 "JR Z" argR8
    0x29 -> inst0 "ADD HL HL"
    0x2a -> inst0 "LD A (HL+)"
    0x2b -> inst0 "DEC HL"
    0x2c -> inst0 "INC L"
    0x2d -> inst0 "DEC L"
    0x2e -> inst1 "LD L" argU8
    0x2f -> inst0 "CPL"

    -- 0x3x
    0x30 -> inst1 "JR NC" argR8
    0x31 -> inst1 "LD SP" argU16
    0x32 -> inst0 "LD (HL-) A"
    0x33 -> inst0 "INC SP"
    0x34 -> inst0 "INC (HL)"
    0x35 -> inst0 "DEC (HL)"
    0x36 -> inst1 "LD (HL)" argU8
    0x37 -> inst0 "SCF"
    0x38 -> inst1 "JR C" argU8
    0x39 -> inst0 "ADD HL SP"
    0x3a -> inst0 "LD A (HL-)"
    0x3b -> inst0 "DEC SP"
    0x3c -> inst0 "INC A"
    0x3d -> inst0 "DEC A"
    0x3e -> inst1 "LD A" argU8
    0x3f -> inst0 "CCF"

    -- 0x4x
    0x40 -> inst0 "LD B B"
    0x41 -> inst0 "LD B C"
    0x42 -> inst0 "LD B D"
    0x43 -> inst0 "LD B E"
    0x44 -> inst0 "LD B H"
    0x45 -> inst0 "LD B L"
    0x46 -> inst0 "LD B (HL)"
    0x47 -> inst0 "LD B A"
    0x48 -> inst0 "LD C B"
    0x49 -> inst0 "LD C C"
    0x4a -> inst0 "LD C D"
    0x4b -> inst0 "LD C E"
    0x4c -> inst0 "LD C H"
    0x4d -> inst0 "LD C L"
    0x4e -> inst0 "LD C (HL)"
    0x4f -> inst0 "LD C A"

    -- 0x5x
    0x50 -> inst0 "LD D B"
    0x51 -> inst0 "LD D C"
    0x52 -> inst0 "LD D D"
    0x53 -> inst0 "LD D E"
    0x54 -> inst0 "LD D H"
    0x55 -> inst0 "LD D L"
    0x56 -> inst0 "LD D (HL)"
    0x57 -> inst0 "LD D A"
    0x58 -> inst0 "LD E B"
    0x59 -> inst0 "LD E C"
    0x5a -> inst0 "LD E D"
    0x5b -> inst0 "LD E E"
    0x5c -> inst0 "LD E H"
    0x5d -> inst0 "LD E L"
    0x5e -> inst0 "LD E (HL)"
    0x5f -> inst0 "LD E A"

    -- 0x6x
    0x60 -> inst0 "LD H B"
    0x61 -> inst0 "LD H C"
    0x62 -> inst0 "LD H D"
    0x63 -> inst0 "LD H E"
    0x64 -> inst0 "LD H H"
    0x65 -> inst0 "LD H L"
    0x66 -> inst0 "LD H (HL)"
    0x67 -> inst0 "LD H A"
    0x68 -> inst0 "LD L B"
    0x69 -> inst0 "LD L C"
    0x6a -> inst0 "LD L D"
    0x6b -> inst0 "LD L E"
    0x6c -> inst0 "LD L H"
    0x6d -> inst0 "LD L L"
    0x6e -> inst0 "LD L (HL)"
    0x6f -> inst0 "LD L A"

    -- 0x7x
    0x70 -> inst0 "LD (HL) B"
    0x71 -> inst0 "LD (HL) C"
    0x72 -> inst0 "LD (HL) D"
    0x73 -> inst0 "LD (HL) E"
    0x74 -> inst0 "LD (HL) H"
    0x75 -> inst0 "LD (HL) L"
    0x76 -> inst0 "HALT"
    0x77 -> inst0 "LD (HL) A"
    0x78 -> inst0 "LD A B"
    0x79 -> inst0 "LD A C"
    0x7a -> inst0 "LD A D"
    0x7b -> inst0 "LD A E"
    0x7c -> inst0 "LD A H"
    0x7d -> inst0 "LD A L"
    0x7e -> inst0 "LD A (HL)"
    0x7f -> inst0 "LD A A"

    -- 0x8x
    0x80 -> inst0 "ADD A B"
    0x81 -> inst0 "ADD A C"
    0x82 -> inst0 "ADD A D"
    0x83 -> inst0 "ADD A E"
    0x84 -> inst0 "ADD A H"
    0x85 -> inst0 "ADD A L"
    0x86 -> inst0 "ADD A (HL)"
    0x87 -> inst0 "ADD A A"
    0x88 -> inst0 "ADC A B"
    0x89 -> inst0 "ADC A C"
    0x8a -> inst0 "ADC A D"
    0x8b -> inst0 "ADC A E"
    0x8c -> inst0 "ADC A H"
    0x8d -> inst0 "ADC A L"
    0x8e -> inst0 "ADC A (HL)"
    0x8f -> inst0 "ADC A A"

    -- 0x9x
    0x90 -> inst0 "SUB A B"
    0x91 -> inst0 "SUB A C"
    0x92 -> inst0 "SUB A D"
    0x93 -> inst0 "SUB A E"
    0x94 -> inst0 "SUB A H"
    0x95 -> inst0 "SUB A L"
    0x96 -> inst0 "SUB A (HL)"
    0x97 -> inst0 "SUB A A"
    0x98 -> inst0 "SBC A B"
    0x99 -> inst0 "SBC A C"
    0x9a -> inst0 "SBC A D"
    0x9b -> inst0 "SBC A E"
    0x9c -> inst0 "SBC A H"
    0x9d -> inst0 "SBC A L"
    0x9e -> inst0 "SBC A (HL)"
    0x9f -> inst0 "SBC A A"

    -- 0xAx
    0xa0 -> inst0 "AND A B"
    0xa1 -> inst0 "AND A C"
    0xa2 -> inst0 "AND A D"
    0xa3 -> inst0 "AND A E"
    0xa4 -> inst0 "AND A H"
    0xa5 -> inst0 "AND A L"
    0xa6 -> inst0 "AND A (HL)"
    0xa7 -> inst0 "AND A A"
    0xa8 -> inst0 "XOR A B"
    0xa9 -> inst0 "XOR A C"
    0xaa -> inst0 "XOR A D"
    0xab -> inst0 "XOR A E"
    0xac -> inst0 "XOR A H"
    0xad -> inst0 "XOR A L"
    0xae -> inst0 "XOR A (HL)"
    0xaf -> inst0 "XOR A A"

    -- 0xBx
    0xb0 -> inst0 "OR A B"
    0xb1 -> inst0 "OR A C"
    0xb2 -> inst0 "OR A D"
    0xb3 -> inst0 "OR A E"
    0xb4 -> inst0 "OR A H"
    0xb5 -> inst0 "OR A L"
    0xb6 -> inst0 "OR A (HL)"
    0xb7 -> inst0 "OR A A"
    0xb8 -> inst0 "CP A B"
    0xb9 -> inst0 "CP A C"
    0xba -> inst0 "CP A D"
    0xbb -> inst0 "CP A E"
    0xbc -> inst0 "CP A H"
    0xbd -> inst0 "CP A L"
    0xbe -> inst0 "CP A (HL)"
    0xbf -> inst0 "CP A A"

    -- 0xCx
    0xc0 -> inst0 "RET NZ"
    0xc1 -> inst0 "POP BC"
    0xc2 -> inst1 "JP NZ" argU16
    0xc3 -> inst1 "JP" argU16
    0xc4 -> inst1 "CALL NZ" argU16
    0xc5 -> inst0 "PUSH BC"
    0xc6 -> inst1 "ADD A" argU8
    0xc7 -> inst0 "RST 0x00"
    0xc8 -> inst0 "RET Z"
    0xc9 -> inst0 "RET"
    0xca -> inst1 "JP Z" argU16
    0xcb -> loadWord8 >>= decodePrefixCbOp
    0xcc -> inst1 "CALL Z" argU16
    0xcd -> inst1 "CALL" argU16
    0xce -> inst1 "ADC A" argU8
    0xcf -> inst0 "RST 0x08"

    -- 0xDx
    0xd0 -> inst0 "RET NC"
    0xd1 -> inst0 "POP DE"
    0xd2 -> inst1 "JP NC" argU16
    0xd3 -> undefined
    0xd4 -> inst1 "CALL NC" argU16
    0xd5 -> inst0 "PUSH DE"
    0xd6 -> inst1 "SUB" argU8
    0xd7 -> inst0 "RST 0x10"
    0xd8 -> inst0 "RET C"
    0xd9 -> inst0 "RETI"
    0xda -> inst1 "JP C" argU16
    0xdb -> undefined
    0xdc -> inst1 "CALL C" argU16
    0xdd -> undefined
    0xde -> inst1 "SBC A" argU8
    0xdf -> inst0 "RST 0x18"

    -- 0xEx
    0xe0 -> inst1' "LDH (" argU8 ") A"
    0xe1 -> inst0 "POP HL"
    0xe2 -> inst0 "LD (C) A"
    0xe3 -> undefined
    0xe4 -> undefined
    0xe5 -> inst0 "PUSH HL"
    0xe6 -> inst1 "AND" argU8
    0xe7 -> inst0 "RST 0x20"
    0xe8 -> inst1 "ADD SP" argR8
    0xe9 -> inst0 "JP (HL)"
    0xea -> inst1' "LD (" argU16 ") A"
    0xeb -> undefined
    0xec -> undefined
    0xed -> undefined
    0xee -> inst1 "XOR" argU8
    0xef -> inst0 "RST 0x28"

    -- 0xFx
    0xf0 -> inst1' "LDH A (" argU8 ")"
    0xf1 -> inst0 "POP AF"
    0xf2 -> inst0 "LD A (C)"
    0xf3 -> inst0 "DI"
    0xf4 -> undefined
    0xf5 -> inst0 "PUSH AF"
    0xf6 -> inst1 "OR" argU8
    0xf7 -> inst0 "RST 0x30"
    0xf8 -> inst1' "LD HL SP" argR8 ""
    0xf9 -> inst0 "LD SP HL"
    0xfa -> inst1' "LD A (" argU16 ")"
    0xfb -> inst0 "EI"
    0xfc -> undefined
    0xfd -> undefined
    0xfe -> inst1 "CP" argU8
    0xff -> inst0 "RST 0x38"

    -- Unreachable, keep the linter happer
    _    -> error . printf "Instruction 0x%02x undefined." $ opCode

 where

  decodePrefixCbOp :: Word8 -> Cpu V.Image
  decodePrefixCbOp opCode =
    case opCode of
      -- 0x0x
      0x00 -> inst0 "RLC B"
      0x01 -> inst0 "RLC C"
      0x02 -> inst0 "RLC D"
      0x03 -> inst0 "RLC E"
      0x04 -> inst0 "RLC H"
      0x05 -> inst0 "RLC L"
      0x06 -> inst0 "RLC (HL)"
      0x07 -> inst0 "RLC A"
      0x08 -> inst0 "RRC B"
      0x09 -> inst0 "RRC C"
      0x0a -> inst0 "RRC D"
      0x0b -> inst0 "RRC E"
      0x0c -> inst0 "RRC H"
      0x0d -> inst0 "RRC L"
      0x0e -> inst0 "RRC (HL)"
      0x0f -> inst0 "RRC A"

      -- 0x1x
      0x10 -> inst0 "RL B"
      0x11 -> inst0 "RL C"
      0x12 -> inst0 "RL D"
      0x13 -> inst0 "RL E"
      0x14 -> inst0 "RL H"
      0x15 -> inst0 "RL L"
      0x16 -> inst0 "RL (HL)"
      0x17 -> inst0 "RL A"
      0x18 -> inst0 "RR B"
      0x19 -> inst0 "RR C"
      0x1a -> inst0 "RR D"
      0x1b -> inst0 "RR E"
      0x1c -> inst0 "RR H"
      0x1d -> inst0 "RR L"
      0x1e -> inst0 "RR (HL)"
      0x1f -> inst0 "RR A"

      -- 0x2x
      0x20 -> inst0 "SLA B"
      0x21 -> inst0 "SLA C"
      0x22 -> inst0 "SLA D"
      0x23 -> inst0 "SLA E"
      0x24 -> inst0 "SLA H"
      0x25 -> inst0 "SLA L"
      0x26 -> inst0 "SLA (HL)"
      0x27 -> inst0 "SLA A"
      0x28 -> inst0 "SRA B"
      0x29 -> inst0 "SRA C"
      0x2a -> inst0 "SRA D"
      0x2b -> inst0 "SRA E"
      0x2c -> inst0 "SRA H"
      0x2d -> inst0 "SRA L"
      0x2e -> inst0 "SRA (HL)"
      0x2f -> inst0 "SRA A"

      -- 0x3x
      0x30 -> inst0 "SWAP B"
      0x31 -> inst0 "SWAP C"
      0x32 -> inst0 "SWAP D"
      0x33 -> inst0 "SWAP E"
      0x34 -> inst0 "SWAP H"
      0x35 -> inst0 "SWAP L"
      0x36 -> inst0 "SWAP (HL)"
      0x37 -> inst0 "SWAP A"
      0x38 -> inst0 "SRL B"
      0x39 -> inst0 "SRL C"
      0x3a -> inst0 "SRL D"
      0x3b -> inst0 "SRL E"
      0x3c -> inst0 "SRL H"
      0x3d -> inst0 "SRL L"
      0x3e -> inst0 "SRL (HL)"
      0x3f -> inst0 "SRL A"

      -- 0x4x
      0x40 -> inst0 "BIT 0 B"
      0x41 -> inst0 "BIT 0 C"
      0x42 -> inst0 "BIT 0 D"
      0x43 -> inst0 "BIT 0 E"
      0x44 -> inst0 "BIT 0 H"
      0x45 -> inst0 "BIT 0 L"
      0x46 -> inst0 "BIT 0 (HL)"
      0x47 -> inst0 "BIT 0 A"
      0x48 -> inst0 "BIT 1 B"
      0x49 -> inst0 "BIT 1 C"
      0x4a -> inst0 "BIT 1 D"
      0x4b -> inst0 "BIT 1 E"
      0x4c -> inst0 "BIT 1 H"
      0x4d -> inst0 "BIT 1 L"
      0x4e -> inst0 "BIT 1 (HL)"
      0x4f -> inst0 "BIT 1 A"

      -- 0x5x
      0x50 -> inst0 "BIT 2 B"
      0x51 -> inst0 "BIT 2 C"
      0x52 -> inst0 "BIT 2 D"
      0x53 -> inst0 "BIT 2 E"
      0x54 -> inst0 "BIT 2 H"
      0x55 -> inst0 "BIT 2 L"
      0x56 -> inst0 "BIT 2 (HL)"
      0x57 -> inst0 "BIT 2 A"
      0x58 -> inst0 "BIT 3 B"
      0x59 -> inst0 "BIT 3 C"
      0x5a -> inst0 "BIT 3 D"
      0x5b -> inst0 "BIT 3 E"
      0x5c -> inst0 "BIT 3 H"
      0x5d -> inst0 "BIT 3 L"
      0x5e -> inst0 "BIT 3 (HL)"
      0x5f -> inst0 "BIT 3 A"

      -- 0x6x
      0x60 -> inst0 "BIT 4 B"
      0x61 -> inst0 "BIT 4 C"
      0x62 -> inst0 "BIT 4 D"
      0x63 -> inst0 "BIT 4 E"
      0x64 -> inst0 "BIT 4 H"
      0x65 -> inst0 "BIT 4 L"
      0x66 -> inst0 "BIT 4 (HL)"
      0x67 -> inst0 "BIT 4 A"
      0x68 -> inst0 "BIT 5 B"
      0x69 -> inst0 "BIT 5 C"
      0x6a -> inst0 "BIT 5 D"
      0x6b -> inst0 "BIT 5 E"
      0x6c -> inst0 "BIT 5 H"
      0x6d -> inst0 "BIT 5 L"
      0x6e -> inst0 "BIT 5 (HL)"
      0x6f -> inst0 "BIT 5 A"

      -- 0x7x
      0x70 -> inst0 "BIT 6 B"
      0x71 -> inst0 "BIT 6 C"
      0x72 -> inst0 "BIT 6 D"
      0x73 -> inst0 "BIT 6 E"
      0x74 -> inst0 "BIT 6 H"
      0x75 -> inst0 "BIT 6 L"
      0x76 -> inst0 "BIT 6 (HL)"
      0x77 -> inst0 "BIT 6 A"
      0x78 -> inst0 "BIT 7 B"
      0x79 -> inst0 "BIT 7 C"
      0x7a -> inst0 "BIT 7 D"
      0x7b -> inst0 "BIT 7 E"
      0x7c -> inst0 "BIT 7 H"
      0x7d -> inst0 "BIT 7 L"
      0x7e -> inst0 "BIT 7 (HL)"
      0x7f -> inst0 "BIT 7 A"

      -- 0x8x
      0x80 -> inst0 "RES 0 B"
      0x81 -> inst0 "RES 0 C"
      0x82 -> inst0 "RES 0 D"
      0x83 -> inst0 "RES 0 E"
      0x84 -> inst0 "RES 0 H"
      0x85 -> inst0 "RES 0 L"
      0x86 -> inst0 "RES 0 (HL)"
      0x87 -> inst0 "RES 0 A"
      0x88 -> inst0 "RES 1 B"
      0x89 -> inst0 "RES 1 C"
      0x8a -> inst0 "RES 1 D"
      0x8b -> inst0 "RES 1 E"
      0x8c -> inst0 "RES 1 H"
      0x8d -> inst0 "RES 1 L"
      0x8e -> inst0 "RES 1 (HL)"
      0x8f -> inst0 "RES 1 A"

      -- 0x9x
      0x90 -> inst0 "RES 2 B"
      0x91 -> inst0 "RES 2 C"
      0x92 -> inst0 "RES 2 D"
      0x93 -> inst0 "RES 2 E"
      0x94 -> inst0 "RES 2 H"
      0x95 -> inst0 "RES 2 L"
      0x96 -> inst0 "RES 2 (HL)"
      0x97 -> inst0 "RES 2 A"
      0x98 -> inst0 "RES 3 B"
      0x99 -> inst0 "RES 3 C"
      0x9a -> inst0 "RES 3 D"
      0x9b -> inst0 "RES 3 E"
      0x9c -> inst0 "RES 3 H"
      0x9d -> inst0 "RES 3 L"
      0x9e -> inst0 "RES 3 (HL)"
      0x9f -> inst0 "RES 3 A"

      -- 0xAx
      0xa0 -> inst0 "RES 4 B"
      0xa1 -> inst0 "RES 4 C"
      0xa2 -> inst0 "RES 4 D"
      0xa3 -> inst0 "RES 4 E"
      0xa4 -> inst0 "RES 4 H"
      0xa5 -> inst0 "RES 4 L"
      0xa6 -> inst0 "RES 4 (HL)"
      0xa7 -> inst0 "RES 4 A"
      0xa8 -> inst0 "RES 5 B"
      0xa9 -> inst0 "RES 5 C"
      0xaa -> inst0 "RES 5 D"
      0xab -> inst0 "RES 5 E"
      0xac -> inst0 "RES 5 H"
      0xad -> inst0 "RES 5 L"
      0xae -> inst0 "RES 5 (HL)"
      0xaf -> inst0 "RES 5 A"

      -- 0xBx
      0xb0 -> inst0 "RES 6 B"
      0xb1 -> inst0 "RES 6 C"
      0xb2 -> inst0 "RES 6 D"
      0xb3 -> inst0 "RES 6 E"
      0xb4 -> inst0 "RES 6 H"
      0xb5 -> inst0 "RES 6 L"
      0xb6 -> inst0 "RES 6 (HL)"
      0xb7 -> inst0 "RES 6 A"
      0xb8 -> inst0 "RES 7 B"
      0xb9 -> inst0 "RES 7 C"
      0xba -> inst0 "RES 7 D"
      0xbb -> inst0 "RES 7 E"
      0xbc -> inst0 "RES 7 H"
      0xbd -> inst0 "RES 7 L"
      0xbe -> inst0 "RES 7 (HL)"
      0xbf -> inst0 "RES 7 A"

      -- 0xCx
      0xc0 -> inst0 "SET 0 B"
      0xc1 -> inst0 "SET 0 C"
      0xc2 -> inst0 "SET 0 D"
      0xc3 -> inst0 "SET 0 E"
      0xc4 -> inst0 "SET 0 H"
      0xc5 -> inst0 "SET 0 L"
      0xc6 -> inst0 "SET 0 (HL)"
      0xc7 -> inst0 "SET 0 A"
      0xc8 -> inst0 "SET 1 B"
      0xc9 -> inst0 "SET 1 C"
      0xca -> inst0 "SET 1 D"
      0xcb -> inst0 "SET 1 E"
      0xcc -> inst0 "SET 1 H"
      0xcd -> inst0 "SET 1 L"
      0xce -> inst0 "SET 1 (HL)"
      0xcf -> inst0 "SET 1 A"

      -- 0xDx
      0xd0 -> inst0 "SET 2 B"
      0xd1 -> inst0 "SET 2 C"
      0xd2 -> inst0 "SET 2 D"
      0xd3 -> inst0 "SET 2 E"
      0xd4 -> inst0 "SET 2 H"
      0xd5 -> inst0 "SET 2 L"
      0xd6 -> inst0 "SET 2 (HL)"
      0xd7 -> inst0 "SET 2 A"
      0xd8 -> inst0 "SET 3 B"
      0xd9 -> inst0 "SET 3 C"
      0xda -> inst0 "SET 3 D"
      0xdb -> inst0 "SET 3 E"
      0xdc -> inst0 "SET 3 H"
      0xdd -> inst0 "SET 3 L"
      0xde -> inst0 "SET 3 (HL)"
      0xdf -> inst0 "SET 3 A"

      -- 0xEx
      0xe0 -> inst0 "SET 4 B"
      0xe1 -> inst0 "SET 4 C"
      0xe2 -> inst0 "SET 4 D"
      0xe3 -> inst0 "SET 4 E"
      0xe4 -> inst0 "SET 4 H"
      0xe5 -> inst0 "SET 4 L"
      0xe6 -> inst0 "SET 4 (HL)"
      0xe7 -> inst0 "SET 4 A"
      0xe8 -> inst0 "SET 5 B"
      0xe9 -> inst0 "SET 5 C"
      0xea -> inst0 "SET 5 D"
      0xeb -> inst0 "SET 5 E"
      0xec -> inst0 "SET 5 H"
      0xed -> inst0 "SET 5 L"
      0xee -> inst0 "SET 5 (HL)"
      0xef -> inst0 "SET 5 A"

      -- 0xFx
      0xf0 -> inst0 "SET 6 B"
      0xf1 -> inst0 "SET 6 C"
      0xf2 -> inst0 "SET 6 D"
      0xf3 -> inst0 "SET 6 E"
      0xf4 -> inst0 "SET 6 H"
      0xf5 -> inst0 "SET 6 L"
      0xf6 -> inst0 "SET 6 (HL)"
      0xf7 -> inst0 "SET 6 A"
      0xf8 -> inst0 "SET 7 B"
      0xf9 -> inst0 "SET 7 C"
      0xfa -> inst0 "SET 7 D"
      0xfb -> inst0 "SET 7 E"
      0xfc -> inst0 "SET 7 H"
      0xfd -> inst0 "SET 7 L"
      0xfe -> inst0 "SET 7 (HL)"
      0xff -> inst0 "SET 7 A"

      -- Unreachable, keep the linter happer
      _    -> error . printf "Instruction 0xcb%02x undefined." $ opCode

  boldText = V.string (attr `V.withStyle` V.bold)
  text' = V.string attr

  argU8 = text' . printf "0x%02x" <$> loadWord8
  argU16 = text' . printf "0x%04x" <$> loadWord16
  argR8 = text' . printf "%+d" <$> loadInt8

  inst0 = return . boldText
  inst1 = fmap . (V.<|>) . boldText . (++ " ")
  inst1' prefix arg suffix = do
    x <- arg
    return $ boldText prefix
      V.<|> x
      V.<|> boldText suffix

  loadWord8 :: Cpu Word8
  loadWord8 = peek . Location . (+1) =<< peek pc

  loadInt8 :: Cpu Int8
  loadInt8 = peek . LocationInt8 . (+1) =<< peek pc

  loadWord16 :: Cpu Word16
  loadWord16 = peek . Location16 . (+1) =<< peek pc

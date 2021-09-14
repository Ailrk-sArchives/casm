{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module As6502.Operand where

import           Data.Word
import Data.Bits (Bits)
-- http://www.obelisk.me.uk/6502/reference.html

-- data Reg = A | X | Y | PC | SP | P deriving (Show, Eq)

data A = A
data X = X
data Y = Y
data PC = PC
data SP = SP
data P = P

data Status = C | Z | I | D | B | V | N deriving (Show, Eq)

newtype Lit = Lit Word8 deriving (Eq, Show, Num, Bits, Ix)

data Mode where
  ACC :: Mode
  ABS :: Mode
  ABSX :: Mode
  ABSY :: Mode
  ZPAGE :: Mode
  ZPAGEX :: Mode
  ZPAGEY :: Mode
  IMM :: Mode
  IND :: Mode
  INDX :: Mode
  INDY :: Mode
  IMPLIED :: Mode
  RELATIVE :: Mode

type family AddrModeType (m :: Mode) asm where
  AddrModeType IMM asm = Lit -> asm
  AddrModeType ZPAGE asm = Word8 -> asm
  AddrModeType ZPAGEX asm = Word8 -> X -> asm
  AddrModeType ABS asm = Word16 -> asm
  AddrModeType ABSX asm = Word16 -> X -> asm
  AddrModeType ABSY asm = Word16 -> Y -> asm


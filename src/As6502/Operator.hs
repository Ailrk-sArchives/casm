{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module As6502.Operator where

-- http://www.6502.org/tutorials/6502opcodes.html
import As6502.Operand
import Data.Word

class Stat2Byte s where
  stat2Byte :: s -> Word8

-- 7 6 5 4 3 2 1 0
-- N V 1 1 D I Z C
instance Stat2Byte Status where
  stat2Byte C = 0x00
  stat2Byte Z = 0x01
  stat2Byte I = 0x02
  stat2Byte D = 0x03
  stat2Byte B = 0x04
  stat2Byte V = 0x06
  stat2Byte N = 0x07

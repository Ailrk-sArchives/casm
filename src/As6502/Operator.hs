{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module As6502.Operator where

-- http://www.6502.org/tutorials/6502opcodes.html
import           ASM.Assembler
import           As6502.Internal
import           As6502.Operand
import           Control.Monad
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Kind
import           Data.Word
import           Internal        (ISALocation)

class Stat2Byte s where
  stat2Byte :: s -> Word8

type Loc = ISALocation As6502

class Load from to where
  ld :: from -> to -> ASM As6502 ()

-- TODO use type family to overload for different addressing modes

type AddrMode (m :: Mode) = AddrModeType m (ASM As6502 ())

class Adc (m :: Mode) f where
  adc :: f

instance (AddrMode ZPAGE ~ f) => Adc ZPAGE f  where
  adc n = undefined

instance (AddrMode ZPAGEX ~ f) => Adc ZPAGEX f  where
  adc n _ = undefined

instance (AddrMode ABS ~ f) => Adc ABS f  where
  adc n = undefined

instance (AddrMode ABSX ~ f) => Adc ABSX f  where
  adc n _ = undefined

instance (AddrMode ABSY ~ f) => Adc ABSY f  where
  adc n _ = undefined

instance (AddrMode IMM ~ f) => Adc IMM f  where
  adc n _ = undefined


bit = undefined

bpl = undefined
bmi = undefined
bvc = undefined
bvs = undefined
bcc = undefined
bcs = undefined
bne = undefined
beq = undefined

brk = undefined

cmp = undefined
cmx = undefined
cmy = undefined

dec = undefined
eor = undefined

clc = undefined
sec = undefined
cli = undefined
sei = undefined
clv = undefined
cld = undefined
sed = undefined

inc = undefined
jmp = undefined
jsr = undefined

lda = undefined
ldx = undefined
ldy = undefined
lsr = undefined

nop = undefined
ora = undefined

tax = undefined
txa = undefined
inx = undefined
tay = undefined
tya = undefined
dey = undefined
iny = undefined

rol = undefined
ror = undefined
rti = undefined
rts = undefined
sbc = undefined

sta = undefined

txs = undefined
tsx = undefined
pha = undefined
pla = undefined
php = undefined
plp = undefined

stx = undefined
sty = undefined


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

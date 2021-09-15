{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
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

type As = ASM As6502 ()

hi :: Word16 -> Word8
hi = fromIntegral . byteSwap16

lo :: Word16 -> Word8
lo = fromIntegral

class Stat2Byte s where
  stat2Byte :: s -> Word8

type Loc = ISALocation As6502

class Adc w r where
  adc :: w -> r

instance Adc Lit As where -- IMM
  adc (Lit w) = code [0x69, w]

instance Adc Word8 As where -- ZP
  adc n = code [0x65, n]

instance Adc Word8 (X -> As) where -- ZPX
  adc n _ = code [0x75, n]

instance Adc Word16 As where  -- ABS
  adc n = code [0x6d, lo n, hi n]

instance Adc Word16 (X -> As) where -- ABSX
  adc n _ = code [0x7d, lo n, hi n]

instance Adc Word16 (Y -> As) where -- ABSY
  adc n _ = code [0x79, lo n, hi n]

instance Adc (Word8, X) As where -- INDX
  adc (n, X) = code [0x61, n]

instance Adc (Word8, Y) As where -- INDY
  adc (n, Y) = code [0x71, n]

class And w r where
  and :: w -> r

instance And Lit As where
  and (Lit w) = code [0x29, w]

instance And Word8 As where
  and n = code [0x25, n]

instance And Word8 (X -> As) where
  and n _ = code [0x35, n]

instance And Word16 As where
  and n = code [0x2d, lo n, hi n]

instance And Word16 (X -> As) where
  and n _ = code [0x3d, lo n, hi n]

instance And Word16 (Y -> As) where
  and n _ = code [0x39, lo n, hi n]

instance And (Word8, X) As where
  and (n, X) = code [0x21, n]

instance And (Word8, Y) As where
  and (n, Y) = code [0x31, n]

class Asl w r where
  asl :: w -> r

instance Asl A As where -- ACC
  asl _ = code [0x69]

instance Asl Word8 As where -- ZP
  asl n = code [0x65, n]

instance Asl Word8 (X -> As) where -- ZPX
  asl n _ = code [0x75, n]

instance Asl Word16 As where  -- ABS
  asl n = code [0x6d, lo n, hi n]

instance Asl Word16 (X -> As) where -- ABSX
  asl n _ = code [0x7d, lo n, hi n]

class Bit w r where
  bit :: w -> r

instance Bit Word8 As where -- ZP
  bit n = code [0x24, n]
instance Bit Word16 As where  -- ABS
  bit n = code [0x2c, lo n, hi n]

-- Branches are all relative addressing  --

bpl :: Word8 -> As
bpl n = code [0x10, n]

bmi :: Word8 -> As
bmi n = code [0x30, n]

bvc :: Word8 -> As
bvc n = code [0x50, n]

bvs :: Word8 -> As
bvs n = code [0x70, n]

bcc :: Word8 -> As
bcc n = code [0x90, n]

bcs :: Word8 -> As
bcs n = code [0xb0, n]

bne :: Word8 -> As
bne n = code [0xd0, n]

beq :: Word8 -> As
beq n = code [0xf0, n]

-- implied
brk :: As
brk = code [0x00]


-- Compare
class Cmp w r where
  cmp :: w -> r

instance Cmp Lit As where -- IMM
  cmp (Lit w) = code [0xc9, w]

instance Cmp Word8 As where -- ZP
  cmp n = code [0xc5, n]

instance Cmp Word8 (X -> As) where -- ZPX
  cmp n _ = code [0xd5, n]

instance Cmp Word16 As where  -- ABS
  cmp n = code [0xcd, lo n, hi n]

instance Cmp Word16 (X -> As) where -- ABSX
  cmp n _ = code [0xdd, lo n, hi n]

instance Cmp Word16 (Y -> As) where -- ABSY
  cmp n _ = code [0xd9, lo n, hi n]

instance Cmp (Word8, X) As where -- INDX
  cmp (n, X) = code [0xc1, n]

instance Cmp (Word8, Y) As where -- INDY
  cmp (n, Y) = code [0xd1, n]


class Cpx w r where
  cpx :: w -> r

instance Cpx Lit As where -- IMM
  cpx (Lit w) = code [0xe0, w]

instance Cpx Word8 As where -- ZP
  cpx n = code [0xe4, n]

instance Cpx Word16 As where  -- ABS
  cpx n = code [0xec, lo n, hi n]


class Cpy w r where
  cpy :: w -> r

instance Cpy Lit As where -- IMM
  cpy (Lit w) = code [0xe0, w]

instance Cpy Word8 As where -- ZP
  cpy n = code [0xe4, n]

instance Cpy Word16 As where  -- ABS
  cpy n = code [0xec, lo n, hi n]


class Dec w r where
  dec :: w -> r

instance Dec Word8 As where -- ZP
  dec n = code [0xc6, n]

instance Dec Word8 (X -> As) where -- ZPX
  dec n _ = code [0xd6, n]

instance Dec Word16 As where  -- ABS
  dec n = code [0xce, lo n, hi n]

instance Dec Word16 (X -> As) where -- ABSX
  dec n _ = code [0xde, lo n, hi n]


class Eor w r where
  eor :: w -> r

instance Eor Lit As where -- IMM
  eor (Lit w) = code [0x49, w]

instance Eor Word8 As where -- ZP
  eor n = code [0x45, n]

instance Eor Word8 (X -> As) where -- ZPX
  eor n _ = code [0x55, n]

instance Eor Word16 As where  -- ABS
  eor n = code [0x4d, lo n, hi n]

instance Eor Word16 (X -> As) where -- ABSX
  eor n _ = code [0x5d, lo n, hi n]

instance Eor Word16 (Y -> As) where -- ABSY
  eor n _ = code [0x59, lo n, hi n]

instance Eor (Word8, X) As where -- INDX
  eor (n, X) = code [0x41, n]

instance Eor (Word8, Y) As where -- INDY
  eor (n, Y) = code [0x51, n]


-- set control flags
clc :: As
clc = code [0x18]

sec :: As
sec = code [0x38]

cli :: As
cli = code [0x58]

sei :: As
sei = code [0x78]

clv :: As
clv = code [0xb8]

cld :: As
cld = code [0xf8]

sed :: As
sed = code [0xf8]


class Inc w r where
  inc :: w -> r

instance Inc Word8 As where -- ZP
  inc n = code [0xc6, n]

instance Inc Word8 (X -> As) where -- ZPX
  inc n _ = code [0xd6, n]

instance Inc Word16 As where  -- ABS
  inc n = code [0xce, lo n, hi n]

instance Inc Word16 (X -> As) where -- ABSX
  inc n _ = code [0xde, lo n, hi n]


class Jmp w where
  jmp :: w -> As

instance Jmp Word16 where  -- ABS
  jmp n = code [0x4c, lo n, hi n]

instance Jmp RelAddr where  -- REL
  jmp (RelAddr n) = code [0x6c, lo n, hi n]


jsr :: Word16 -> As
jsr h = code [0x20, lo h, hi h]


-- Load
class Lda w r where
  lda :: w -> r

instance Lda Lit As where -- IMM
  lda (Lit w) = code [0xa9, w]

instance Lda Word8 As where -- ZP
  lda n = code [0xa5, n]

instance Lda Word8 (X -> As) where -- ZPX
  lda n _ = code [0xb5, n]

instance Lda Word16 As where  -- ABS
  lda n = code [0xad, lo n, hi n]

instance Lda Word16 (X -> As) where -- ABSX
  lda n _ = code [0xbd, lo n, hi n]

instance Lda Word16 (Y -> As) where -- ABSY
  lda n _ = code [0xb9, lo n, hi n]

instance Lda (Word8, X) As where -- INDX
  lda (n, X) = code [0xa1, n]

instance Lda (Word8, Y) As where -- INDY
  lda (n, Y) = code [0xb1, n]


class Ldx w r where
  ldx :: w -> r

instance Ldx Lit As where -- IMM
  ldx (Lit w) = code [0xa2, w]

instance Ldx Word8 As where -- ZP
  ldx n = code [0xa6, n]

instance Ldx Word8 (Y -> As) where -- ZPY
  ldx n _ = code [0xb6, n]

instance Ldx Word16 As where  -- ABS
  ldx n = code [0xae, lo n, hi n]

instance Ldx Word16 (Y -> As) where -- ABSY
  ldx n _ = code [0xbe, lo n, hi n]


class Ldy w r where
  ldy :: w -> r

instance Ldy Lit As where -- IMM
  ldy (Lit w) = code [0xa0, w]

instance Ldy Word8 As where -- ZP
  ldy n = code [0xa4, n]

instance Ldy Word8 (X -> As) where -- ZPX
  ldy n _ = code [0xb4, n]

instance Ldy Word16 As where  -- ABS
  ldy n = code [0xac, lo n, hi n]

instance Ldy Word16 (X -> As) where -- ABSX
  ldy n _ = code [0xbc, lo n, hi n]


class Lsr w r where
  lsr :: w -> r

instance Lsr Lit As where -- IMM
  lsr (Lit w) = code [0x4a, w]

instance Lsr Word8 As where -- ZP
  lsr n = code [0x46, n]

instance Lsr Word8 (X -> As) where -- ZPX
  lsr n _ = code [0x56, n]

instance Lsr Word16 As where  -- ABS
  lsr n = code [0x4e, lo n, hi n]

instance Lsr Word16 (X -> As) where -- ABSX
  lsr n _ = code [0x5e, lo n, hi n]


nop :: As
nop = code [0xea]

class Ora w r where
  ora :: w -> r

instance Ora Lit As where -- IMM
  ora (Lit w) = code [0x09, w]

instance Ora Word8 As where -- ZP
  ora n = code [0x05, n]

instance Ora Word8 (X -> As) where -- ZPX
  ora n _ = code [0x15, n]

instance Ora Word16 As where  -- ABS
  ora n = code [0x0d, lo n, hi n]

instance Ora Word16 (X -> As) where -- ABSX
  ora n _ = code [0x1d, lo n, hi n]

instance Ora Word16 (Y -> As) where -- ABSY
  ora n _ = code [0x19, lo n, hi n]

instance Ora (Word8, X) As where -- INDX
  ora (n, X) = code [0x01, n]

instance Ora (Word8, Y) As where -- INDY
  ora (n, Y) = code [0x11, n]


-- Register instructions (all implied)
tax :: As
tax = code [0xaa]

txa :: As
txa = code [0x8a]

dex :: As
dex = code [0xca]

inx :: As
inx = code [0xe8]

tay :: As
tay = code [0xa8]

tya :: As
tya = code [0x98]

dey :: As
dey = code [0x88]

iny :: As
iny = code [0xc8]

class Ror w r where
  ror :: w -> r

instance Ror Lit As where -- IMM
  ror (Lit w) = code [0x2a, w]

instance Ror Word8 As where -- ZP
  ror n = code [0x26, n]

instance Ror Word8 (X -> As) where -- ZPX
  ror n _ = code [0x36, n]

instance Ror Word16 As where  -- ABS
  ror n = code [0x2e, lo n, hi n]

instance Ror Word16 (X -> As) where -- ABSX
  ror n _ = code [0x3e, lo n, hi n]


class Rol w r where
  rol :: w -> r

instance Rol Word8 As where -- ZP
  rol n = code [0x26, n]

instance Rol Word8 (X -> As) where -- ZPX
  rol n _ = code [0x36, n]

instance Rol Word16 As where  -- ABS
  rol n = code [0x2e, lo n, hi n]

instance Rol Word16 (X -> As) where -- ABSX
  rol n _ = code [0x3e, lo n, hi n]


rti :: As
rti = code [0x40]

rsi :: As
rsi = code [0x60]

class Sbc w r where
  sbc :: w -> r

instance Sbc Lit As where -- IMM
  sbc (Lit w) = code [0xe9, w]

instance Sbc Word8 As where -- ZP
  sbc n = code [0xe5, n]

instance Sbc Word8 (X -> As) where -- ZPX
  sbc n _ = code [0xf5, n]

instance Sbc Word16 As where  -- ABS
  sbc n = code [0xed, lo n, hi n]

instance Sbc Word16 (X -> As) where -- ABSX
  sbc n _ = code [0xfd, lo n, hi n]

instance Sbc Word16 (Y -> As) where -- ABSY
  sbc n _ = code [0xf9, lo n, hi n]

instance Sbc (Word8, X) As where -- INDX
  sbc (n, X) = code [0xe1, n]

instance Sbc (Word8, Y) As where -- INDY
  sbc (n, Y) = code [0xf1, n]


class Sta w r where
  sta :: w -> r

instance Sta Word8 As where -- ZP
  sta n = code [0x85, n]

instance Sta Word8 (X -> As) where -- ZPX
  sta n _ = code [0x85, n]

instance Sta Word16 As where  -- ABS
  sta n = code [0x8d, lo n, hi n]

instance Sta Word16 (X -> As) where -- ABSX
  sta n _ = code [0x9d, lo n, hi n]

instance Sta Word16 (Y -> As) where -- ABSY
  sta n _ = code [0x99, lo n, hi n]

instance Sta (Word8, X) As where -- INDX
  sta (n, X) = code [0x81, n]

instance Sta (Word8, Y) As where -- INDY
  sta (n, Y) = code [0x91, n]


txs :: As
txs = code [0x9a]

tsx :: As
tsx = code [0xba]

pha :: As
pha = code [0x48]

pla :: As
pla = code [0x68]

php :: As
php = code [0x08]

plp :: As
plp = code [0x28]

class Stx w r where
  stx :: w -> r

instance Stx Word8 As where -- ZP
  stx n = code [0x86, n]

instance Stx Word8 (X -> As) where -- ZPX
  stx n _ = code [0x96, n]

instance Stx Word16 As where  -- ABS
  stx n = code [0x83, lo n, hi n]


class Sty w r where
  sty :: w -> r

instance Sty Word8 As where -- ZP
  sty n = code [0x84, n]

instance Sty Word8 (X -> As) where -- ZPX
  sty n _ = code [0x94, n]

instance Sty Word16 As where  -- ABS
  sty n = code [0x8c, lo n, hi n]


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

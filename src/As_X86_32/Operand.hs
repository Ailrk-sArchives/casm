{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeFamilies               #-}
module As_X86_32.Operand where

import           Data.Bits (Bits)
import           Data.Word

-- 32 bit regs
data EAX = EAX
data EBX = EBX
data ECX = ECX
data EDX = EDX

data ESP = ESP
data EBP = EBP
data ESI = ESI
data EDI = EDI

-- 16 bit reg
data AX = AX
data BX = BX
data CX = CX
data DX = DX

data SP = SP
data BP = BP
data SI = SI
data DI = DI

-- 8 bit hi regs
data AH = AH
data BH = BH
data CH = CH
data DH = DH

data SPL = SPL
data BPL = BPL
data SIL = SIL
data DIL = DIL

-- segment regs
data SS = SS    -- stack seg
data CS = CS    -- code seg
data DS = DS    -- data seg
data ES = ES    -- extra seg
data FS = FS    -- more extra seg
data GS = GS    -- more more extra seg

-- flag reg
data EFLAG = EFLAG

data EFLAGS
  = CF        -- carray
  | EMPTY_1
  | PF        -- parity
  | EMPTY_4
  | AF        -- adjust
  | EMPTY_5
  | ZF        -- zero
  | SF        -- sign
  | TF        -- trap
  | IF        -- interruption
  | DF        -- direction
  | OF        -- overflow
  | IOPL1     -- io privilege level
  | IOPL2
  | NT
  | EMPTY_15  -- nested task
  | RF        -- resume
  | VM        -- virtual 8086
  | AC        -- Alignment check
  | VIF       -- Virtual interrupt
  | VIP       -- Virtual interrupt pending
  | ID        -- identification flag (CPUID)

  deriving (Enum, Eq, Show)

-- instruction pointer
data EIP = EIP

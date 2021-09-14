{-# LANGUAGE DataKinds #-}
module As8051.Operand where

import           Data.Word


-- all registers on 8051 are 8 bit except DPTR and PC.

data Reg
  = A | B | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | PSW
  -- dph and dpl are upper and lower half of dptr
  | DPTR | DPH | DPL
  | PC
  deriving (Show, Eq)

data Flag = CY | AC | F0 | RS1 | RS0 | OV | U_DEFINED | P deriving (Show, Eq)


-- RS0 and RS1 can be used to select register bank.

type RegBank = Int
type Address = Int
registerBank :: (Int, Int) -> (RegBank, (Address, Address))
registerBank (0, 0) = (0, (0x00, 0x07))
registerBank (0, 1) = (1, (0x08, 0x0f))
registerBank (1, 0) = (2, (0x10, 0x17))
registerBank (1, 1) = (3, (0x18, 0x1f))

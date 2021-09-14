{-# LANGUAGE DataKinds #-}
module As6502.Operand where

import Data.Word
-- http://www.obelisk.me.uk/6502/reference.html

data Reg = A | X | Y | PC | SP | P deriving (Show, Eq)
data Status = C | Z | I | D | B | V | N deriving (Show, Eq)
data AddrMode =
  IMM | ZPAGE | ZPAGEX | ABS | ABSX | ABSY | INDX | INDY | IMPLIED
  deriving (Show, Eq)

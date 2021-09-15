module Test where

import           ASM.Assembler
import           As6502.Internal
import           As6502.Operand
import           As6502.Operator
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS
import           Data.Word
import           Debug.Trace
import           Numeric

w :: (Integral n) => n -> Word8
w n = fromIntegral n

dw :: (Integral n) => n -> Word16
dw n = fromIntegral n

toHex = mconcat . fmap (flip showHex " ") . BS.unpack

test1 :: (ASMState As6502, ByteString)
test1 = runASM $ do
  clc
  sec
  clc
  adc (dw 11) Y

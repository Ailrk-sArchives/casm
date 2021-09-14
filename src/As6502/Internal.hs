{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module As6502.Internal where

import           Data.Word
import Internal

data As6502
type instance ISALocation As6502 = Word8

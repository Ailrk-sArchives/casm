{-# LANGUAGE TypeFamilies #-}
module As_X86_32.Internal where

import           Data.Word
import           Internal

data As_X86_32
type instance ISALocation As_X86_32 = Word32

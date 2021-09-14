{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module As8051.Internal where


import           Data.Word
import           Internal

data As8051
type instance ISALocation As8051 = Word8


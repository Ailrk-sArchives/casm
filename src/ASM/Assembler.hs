{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module As6502.Assembler where

import           Control.Monad.RWS
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Data.Word


data ASMState = ASMState
  { location :: Word8
  , entry    :: Maybe Word8
  }

newtype ASM isa a = ASM { unASM :: RWS () ByteString ASMState a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadWriter ByteString
    , MonadState ASMState )

-- bdb
class Bytes a where
  db :: a -> ASM isa ()

nextLocation :: Word8 -> ASMState -> ASMState
nextLocation offset s = s { location = location s + offset }

code ::  [Word8] -> ASM isa ()
code bs = do
  tell $ BS.pack bs
  modify next
  where
    next = nextLocation . fromIntegral $ length bs

-- entrance
runASM :: ASM isa ()
runASM = undefined

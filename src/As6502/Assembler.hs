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

newtype As6502 a = As6502 { unAs6502 :: RWS () ByteString ASMState a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadWriter ByteString
    , MonadState ASMState )

-- bdb
class Bytes a where
  db :: a -> As6502 ()

nextLocation :: Word8 -> ASMState -> ASMState
nextLocation offset s = s { location = location s + offset }

input ::  [Word8] -> As6502 ()
input bs = do
  tell $ BS.pack bs
  modify next
  where
    next = nextLocation . fromIntegral $ length bs


-- entrance
runAs6502 :: As6502 ()
runAs6502 = undefined

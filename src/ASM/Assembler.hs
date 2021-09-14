{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
module ASM.Assembler where

import           Control.Monad.RWS
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Data.Foldable     (traverse_)
import           Data.Word
import           Internal


data ASMState isa = ASMState
  { location :: ISALocation isa
  , entry    :: Maybe Word8
  }

newtype ASM isa a = ASM { unASM :: RWS () ByteString (ASMState isa) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadWriter ByteString
    , MonadState (ASMState isa) )

-- small asm state
data CodeBlock isa = ASMBlock
  { cborg   :: ISALocation isa
  , cbentry :: ISALocation isa
  , cbdata  :: ByteString
  }

type ISA a = (Num (ISALocation a))

-- bdb
class Bytes a where
  defineByte :: ISA isa => a -> ASM isa ()

instance Bytes ByteString where
  defineByte bs = ASM (tell bs >> modify inc)
    where
      inc = incLocation . fromIntegral $ BS.length bs

instance Bytes Word8 where
  defineByte b = defineByte (BS.singleton b)

instance Bytes n => Bytes [n] where
  defineByte = traverse_ defineByte

incLocation :: ISA isa => ISALocation isa -> ASMState isa  -> ASMState isa
incLocation offset s = s { location = location s + offset }

code :: ISA isa => [Word8] -> ASM isa ()
code bs = do
  tell $ BS.pack bs
  modify inc
  where
    inc = incLocation . fromIntegral $ length bs

-- byte :: (Bytes a, Num (ISALocation a)) => a -> ASM isa ()


byte :: (Bytes a, ISA isa) => a -> ASM isa ()
byte = defineByte

byte2 :: (Bytes a, ISA isa) => a -> ASM isa ()
byte2 = undefined

byte4 :: (Bytes a, ISA isa) => a -> ASM isa ()
byte4 = undefined

byte8 :: (Bytes a, ISA isa) => a -> ASM isa ()
byte8 = undefined

abort :: (ISA isa) => String -> ASM isa ()
abort = undefined

ascii :: (Bytes a, ISA isa) => a -> ASM isa ()
ascii = undefined

asciiz :: (Bytes a, ISA isa) => a -> ASM isa ()
asciiz = undefined

align :: (Bytes a, ISA isa) => a -> ASM isa ()
align = undefined

label :: ASM isa (ISALocation isa)
label = location <$> get

withLabel :: (ISA isa) => (ISALocation isa -> ASM isa a) -> ASM isa a
withLabel = (label >>=)

org :: (ISA isa) => ISALocation isa -> ASM isa a -> CodeBlock isa
org = undefined

equ :: a -> ASM isa a
equ = return

-- entrance
runASM :: ASM isa ()
runASM = undefined

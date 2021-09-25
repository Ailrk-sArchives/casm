{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Util where

import           Data.Bits
import           Data.Maybe
import           Data.Proxy
import           Data.Word
import           GHC.TypeLits

w :: (Integral n) => n -> Word8
w n = fromIntegral n

dw :: (Integral n) => n -> Word16
dw n = fromIntegral n

lw :: (Integral n) => n -> Word64
lw n = fromIntegral n

qw :: (Integral n) => n -> Word64
qw n = fromIntegral n

-- | get the higher and lower half of a word.
class ChopWord bigger smaller where
  hi :: bigger -> smaller
  lo :: bigger -> smaller

instance ChopWord Word16 Word8 where
  hi = fromIntegral . byteSwap16
  lo = fromIntegral

instance ChopWord Word32 Word16 where
  hi = fromIntegral . byteSwap32
  lo = fromIntegral

instance ChopWord Word64 Word32 where
  hi = fromIntegral . byteSwap64
  lo = fromIntegral

-- | word literals type
-- each branch only takes value it supports.
--
--(KnownNat a, (a <= 0xff)) =>
data WordLit r where
  W8 :: Word8 -> WordLit r
  W16 :: Word16 -> WordLit r
  W32 :: Word32 -> WordLit r
  W64 :: Word64 -> WordLit r

deriving instance Show (WordLit r)

-- we want to

fromInteger2Nat n =
  case someNatVal n of
    Just (SomeNat (_ :: Proxy n)) ->
      if | n <= 0xff               -> W8 (fromIntegral n)
         | n <= 0xffff             -> W16 (fromIntegral n)
         | n <= 0xfffffffff        -> W32 (fromIntegral n)
         | n <= 0xffffffffffffffff -> W64 (fromIntegral n)
    _                             ->  error "can't"

instance Num (WordLit r) where
  fromInteger = fromInteger2Nat

  W8 n + W8 m   = W8 $ (n + m)
  W16 n + W16 m = W16 $ (n + m)
  W32 n + W32 m = W32 $ (n + m)
  W64 n + W64 m = W64 $ (n + m)

  negate (W8 n)  = W8 $ negate n
  negate (W16 n) = W16 $ negate n
  negate (W32 n) = W32 $ negate n
  negate (W64 n) = W64 $ negate n

  abs (W8 n)  = W8 $ abs n
  abs (W16 n) = W16 $ abs n
  abs (W32 n) = W32 $ abs n
  abs (W64 n) = W64 $ abs n

-- a :: Word8
-- a = fromWordLit (W8 0xff)

-- type level coputation dumps to term
--
-- Type => Term
add :: KnownNat n => Proxy n -> Proxy n -> Integer
add a b = let a1 = natVal a
              b1 = natVal b
           in fromIntegral $ a1 + b1

-- Term => Type

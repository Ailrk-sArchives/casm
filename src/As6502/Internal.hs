{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module As6502.Internal where

import           Data.Bits
import           Data.Word
import           Internal

data As6502
type instance ISALocation As6502 = Word8


-- goal:
-- 1. when it's 1,2 digit numbers convert to Word8
-- 2. when it's 3,4 digit numbers convert to Word16
-- 3. other cases simply throw error


data As6502Lit r where
  Lit8 :: Word8 -> As6502Lit r
  Lit16 :: Word16 -> As6502Lit r
  deriving (Eq, Ord)

instance Enum (As6502Lit r) where
  toEnum n
    | n >= 0 && n <= 0xff = Lit8 (fromIntegral n)
    | n > 0xff && n <= 0xffff = Lit16 (fromIntegral n)
    | otherwise = error "6502 support maximum 0xffff"
  fromEnum (Lit8 n)  = fromEnum n
  fromEnum (Lit16 n) = fromEnum n

instance Ord (As6502Lit r) => Real (As6502Lit r) where
  toRational n = error "no rational"

instance Integral (As6502Lit r) where
  (Lit8 n) `quotRem` (Lit8 m) = let (a, b) = n `quotRem` m
                                 in (Lit8 a, Lit8 b)
  (Lit16 n) `quotRem` (Lit16 m) = let (a, b) = n `quotRem` m
                                 in (Lit16 a, Lit16 b)
  (Lit16 n) `quotRem` (Lit8 m) = let (a, b) = n `quotRem` (fromIntegral m)
                                 in (Lit16 a, Lit16 b)
  _ `quotRem` _ = error "illegal modulo"
  toInteger as = case as of
                Lit8 n  -> toInteger n
                Lit16 n -> toInteger n

instance Num (As6502Lit r) where
  fromInteger n
    | n >= 0 && n <= 0xff = Lit8 (fromIntegral n)
    | n > 0xff && n <= 0xffff = Lit16 (fromIntegral n)
    | otherwise = error "6502 support maximum 0xffff"

  abs (Lit8 n) = Lit8 (abs n)
  abs (Lit16 n) = Lit16 (abs n)

  negate (Lit8 n)  = Lit8 $ negate n
  negate (Lit16 n) = Lit16 $ negate n
  (Lit8 n) + (Lit8 m)   = Lit8 $ n + m
  (Lit16 n) + (Lit16 m) = Lit16 $ n + m
  _ + _                 = error "illegal addition"

bar foo = case foo of
        Lit8 _  -> putStrLn "lit8"
        Lit16 _ -> putStrLn "lit17"

foo :: As6502Lit r
foo = 0xfff1

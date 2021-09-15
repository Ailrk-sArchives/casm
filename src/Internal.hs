{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}

module Internal where

import           Data.Word

-- | give location
--   TODO find a place to define this, and

-- type ISALocation :: Type -> Num
type family ISALocation isa


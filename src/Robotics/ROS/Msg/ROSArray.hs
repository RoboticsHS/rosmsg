{-# LANGUAGE KindSignatures, DataKinds #-}
module Robotics.ROS.Msg.ROSArray
  ( ROSArray(..)
  , ROSFixedArray(..)
  ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Binary (Binary(..), Get)
import Control.Monad (replicateM)
import Data.Word (Word32)

type Array = []
-- TODO: migrate to more performance vector type

-- |A type for arrays in ROS messages
newtype ROSArray a = ROSArray { unArray :: Array a }
  deriving (Show, Eq, Ord)

-- Array monoid
instance Monoid (ROSArray a) where
    mempty = ROSArray []
    mappend a b = ROSArray (unArray a ++ unArray b)

-- Array serialization
instance Binary a => Binary (ROSArray a) where
    put (ROSArray arr) = put len >> sequence_ (put <$> arr)
      where len :: Word32
            len = fromIntegral (length arr)

    get = do len <- get :: Get Word32
             ROSArray <$> replicateM (fromIntegral len) get

-- |A type for fixed arrays in ROS messages
newtype ROSFixedArray a (n :: Nat) = ROSFixedArray { unFixedArray :: Array a }
  deriving (Show, Eq, Ord)

-- Fixed array monoid
instance Monoid (ROSFixedArray a n) where
    mempty = ROSFixedArray []
    mappend a b = ROSFixedArray (unFixedArray a ++ unFixedArray b)

-- Fixed array serialization
instance (Binary a, KnownNat n) => Binary (ROSFixedArray a n) where
    put (ROSFixedArray arr) = sequence_ (put <$> arr)

    get = let arr = mempty in modify arr <$> replicateM (len arr) get 
      where modify :: ROSFixedArray a n -> Array b -> ROSFixedArray b n 
            modify a x = a { unFixedArray = x }
            len = fromIntegral . natVal

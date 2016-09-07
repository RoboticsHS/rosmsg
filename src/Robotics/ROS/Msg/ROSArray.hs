{-# LANGUAGE KindSignatures, DataKinds, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Robotics.ROS.Msg.ROSArray
  ( ROSArray(..)
  , ROSFixedArray(..)
  ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Binary (Binary(..), Get)
import Control.Monad (replicateM)
import Data.Default (Default(..))
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Data.Data (Data)

type Array = []
-- TODO: migrate to more performance vector type

-- |A type for arrays in ROS messages
newtype ROSArray a = ROSArray { unArray :: Array a }
  deriving (Show, Eq, Ord, Data, Typeable, Functor)

-- |Array monoid
instance Monoid (ROSArray a) where
    mempty = ROSArray []
    mappend a b = ROSArray (unArray a ++ unArray b)

-- |Array default is empty
instance Default (ROSArray a) where
    def = mempty

-- |Array serialization
instance Binary a => Binary (ROSArray a) where
    put (ROSArray arr) = put len >> sequence_ (put <$> arr)
      where len :: Word32
            len = fromIntegral (length arr)

    get = do len <- get :: Get Word32
             ROSArray <$> replicateM (fromIntegral len) get

-- |A type for fixed arrays in ROS messages
newtype ROSFixedArray (n :: Nat) a = ROSFixedArray { unFixedArray :: Array a }
  deriving (Show, Eq, Ord, Data, Typeable, Functor)

-- |Fixed array monoid
instance Monoid (ROSFixedArray a n) where
    mempty = ROSFixedArray []
    mappend a b = ROSFixedArray (unFixedArray a ++ unFixedArray b)

size :: (KnownNat n, Num b) => ROSFixedArray n a -> b
size = fromIntegral . natVal . proxy
  where proxy :: ROSFixedArray n a -> proxy n
        proxy _ = undefined

modify :: ROSFixedArray n a -> Array b -> ROSFixedArray n b
modify a b = a { unFixedArray = b }

instance (Default a, KnownNat n) => Default (ROSFixedArray n a) where
    -- |Fixed array default list of `n` default elements
    def = modify arr (replicate (size arr) def)
      where arr = mempty

-- |Fixed array serialization
instance (Binary a, KnownNat n) => Binary (ROSFixedArray n a) where
    put (ROSFixedArray arr) = sequence_ (put <$> arr)
    get = modify arr <$> replicateM (size arr) get
      where arr = mempty

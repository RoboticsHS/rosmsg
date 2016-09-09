module Robotics.ROS.Msg
  ( module Robotics.ROS.Msg.ROSArray
  , Message(..)
  , Stamped(..)
  , ROSDuration
  , ROSTime
  ) where

import Data.Digest.Pure.MD5 (MD5Digest) 
import Data.ByteString (ByteString)
import Data.Binary (Binary)
import Data.Word (Word32)
import Data.Text (Text)

import Robotics.ROS.Msg.ROSArray
import Robotics.ROS.Msg.Types

-- |ROS message
class Binary a => Message a where
    getDigest :: a -> MD5Digest
    getType   :: a -> Text

-- |ROS message with Header
class Message a => Stamped a where
    getSequence :: a -> Word32
    setSequence :: Word32 -> a -> a
    getStamp    :: a -> ROSTime
    getFrame    :: a -> ByteString

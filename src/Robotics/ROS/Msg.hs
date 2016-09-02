module Robotics.ROS.Msg
  ( module Robotics.ROS.Msg.ROSArray
  , module Lens.Simple
  , Message(..)
  , Stamped(..)
  , ROSDuration
  , ROSTime
  ) where

import Data.Digest.Pure.MD5 (MD5Digest) 
import Data.Binary (Binary)
import Data.Word (Word32)
import Data.Text (Text)
import Lens.Simple

import Robotics.ROS.Msg.ROSArray
import Robotics.ROS.Msg.Types

class Binary a => Message a where
    getDigest :: a -> MD5Digest
    getType   :: a -> Text

class Message a => Stamped a where
    getSequence :: a -> Word32
    setSequence :: Word32 -> a -> a
    getStamp    :: a -> ROSTime
    getFrame    :: a -> Text

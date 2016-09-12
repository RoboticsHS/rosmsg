-- |
-- Module      :  Robotics.ROS.Msg.Class
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- ROS message type class declaration.
--
module Robotics.ROS.Msg.Class (
    Message(..)
  , Stamped(..)
  ) where

import Robotics.ROS.Msg.Types (ROSTime)
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.ByteString (ByteString)
import Data.Binary (Binary)
import Data.Word (Word32)
import Data.Text (Text)

-- | Haskell native type for ROS message language described
-- data structure. Serialization guaranted by 'Binary' super
-- class. And no more is needed for transfer over socket.
class Binary a => Message a where
    -- | Get message type string, e.g. @std_msgs/Char@
    getType   :: a -> Text

    -- | Get message source
    getSource :: a -> Text

    -- | Get recurrent MD5 of message source
    getDigest :: a -> MD5Digest

-- | Sometime ROS messages have a special @Header@ field.
-- It used for tracking package sequence, time stamping
-- and frame tagging. Headers is frequently field. The
-- 'Stamped' type class lifts header fields on the top
-- of message and abstracting of type.
class Message a => Stamped a where
    -- | Get sequence number
    getSequence :: a -> Word32

    -- | Set sequence number
    setSequence :: Word32 -> a -> a

    -- | Get timestamp of message
    getStamp    :: a -> ROSTime

    -- | Get frame of message
    getFrame    :: a -> ByteString

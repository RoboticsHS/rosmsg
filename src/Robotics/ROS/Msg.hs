-- |
-- Module      :  Robotics.ROS.Msg
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Robot Operating System (ROS) is a set of software libraries and tools
-- that help you build robot applications. From drivers to state-of-the-art
-- algorithms, and with powerful developer tools, ROS has what you need for
-- your next robotics project. And it's all open source.
--
-- In ROS a node is a process that performs computation. 
-- Nodes communicate with each other by publishing messages to topics.
-- A message is a simple data structure, comprising typed fields. Standard
-- primitive types (integer, floating point, boolean, etc.) are supported,
-- as are arrays of primitive types. Messages can include arbitrarily nested
-- structures and arrays (much like C structs).
--
-- This package provide the ROS message language parser and builder. Abstract
-- message representation given by parser can be used in TemplateHaskell codegen
-- for native Haskell structures creation.
--
-- >>> [rosmsgFrom|/opt/ros/jade/share/std_msgs/msg/Byte.msg|]
-- "[Variable (Simple RByte,\"data\")]"
--
-- >>> [rosmsgFrom|/opt/ros/jade/share/geometry_msgs/msg/Polygon.msg|]
-- "[Variable (Array (Custom \"Point32\"),\"points\")]"
--
module Robotics.ROS.Msg (
  -- * Message classes 
    Message(..)
  , Stamped(..)
  -- * Common used types
  -- ** Array-like types
  , ROSFixedArray(..)
  , ROSArray(..)
  -- ** Time description
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

-- | Haskell native type for ROS message language described
-- data structure. Serialization guaranted by 'Binary' super
-- class. And no more is needed for transfer over socket.
class Binary a => Message a where
    -- | Get MD5 of formal message representation
    getDigest :: a -> MD5Digest
    -- | Get message type, e.g. @std_msgs/Char@
    getType   :: a -> Text

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

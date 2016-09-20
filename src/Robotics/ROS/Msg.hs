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
module Robotics.ROS.Msg (
  -- * ROS message classes
    Message(..)
  , Stamped(..)
  -- * Common used types
  -- ** Array-like
  , ROSFixedArray(..)
  , ROSArray(..)
  -- ** Time description
  , ROSDuration
  , ROSTime
  ) where

import Robotics.ROS.Msg.ROSArray
import Robotics.ROS.Msg.Types
import Robotics.ROS.Msg.Class

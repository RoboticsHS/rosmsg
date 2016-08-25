module Robotics.ROS.Msg
  ( Message(..)
  , module Robotics.ROS.Msg.HasHeader 
  , module Robotics.ROS.Msg.Types
  ) where

import Robotics.ROS.Msg.HasHeader
import Robotics.ROS.Msg.Types
import Data.Binary (Binary)

class Binary a => Message a where
    checkSum :: a -> String
    typeName :: a -> String

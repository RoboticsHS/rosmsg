-- |If a message type's first field is of type Header, its sequence
-- number is automatically incremented by the ROS Topic machinery.
module Robotics.ROS.Msg.HasHeader where

import Robotics.ROS.Msg.Types (ROSTime)
import Data.Binary (Binary, Put, put)
import Data.Word (Word32)

class Binary a => HasHeader a where
    getSequence :: a -> Word32
    getFrame    :: a -> String
    getStamp    :: a -> ROSTime
    setSequence :: Word32 -> a -> a

-- |Serialize a message after setting the sequence number in its
-- header.
putStampedMsg :: HasHeader a => Word32 -> a -> Put
putStampedMsg n v = put $ setSequence n v

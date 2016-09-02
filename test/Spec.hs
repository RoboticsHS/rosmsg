{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric, DeriveDataTypeable, QuasiQuotes, OverloadedStrings #-}
module Robotics.ROS.Msg.Geometry_msgs.Pose2D where

import Robotics.ROS.Msg.TH
import Robotics.ROS.Msg

import Data.Digest.Pure.MD5
import GHC.Generics
import Data.Typeable
import Data.Default
import Data.Binary
import Data.Data
import Data.Word
import Data.Int

[rosmsgFrom|/usr/share/geometry_msgs/msg/Pose2D.msg|]

main :: IO ()
main = putStrLn $ show $ encode (def :: Pose2D)

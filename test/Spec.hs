{-# LANGUAGE DataKinds, KindSignatures, DeriveGeneric, DeriveDataTypeable, QuasiQuotes, OverloadedStrings #-}
module Main where

import Robotics.ROS.Msg.TH
import Robotics.ROS.Msg

import Data.Digest.Pure.MD5
import GHC.Generics
import Data.ByteString.Lazy.Char8 (pack)
import Text.Printf
import Data.Typeable
import Data.Default
import Data.Binary
import Data.Data
import Data.Word
import Data.Int

-- [rosmsgFrom|/usr/share/geometry_msgs/msg/PoseWithCovariance.msg|]

main :: IO ()
main = putStrLn "Test suite not yet implemented"

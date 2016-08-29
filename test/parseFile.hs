module Main (main) where

import Robotics.ROS.Msg.Parser

import Data.Text.Lazy.IO as TL
import System.IO (stdin)

main :: IO ()
main = do
  txt <- TL.hGetContents stdin
  print $ parse msgParser txt

# ROS message parser / render / .hs generator

[![Build Status](https://travis-ci.org/RoboticsHS/rosmsg.svg?branch=master)](https://travis-ci.org/RoboticsHS/rosmsg)
[![Build status](https://ci.appveyor.com/api/projects/status/cice533o4q04e0yd?svg=true)](https://ci.appveyor.com/project/akru/rosmsg)
![Hackage](https://img.shields.io/hackage/v/rosmsg.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/rosmsg.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

## Install

    $ git clone https://github.com/RoboticsHS/rosmsg
    $ cd rosmsg
    $ stack ghci

## Fun

### Parse / render

    > let Done _ msg = parse Robotics.ROS.Msg.Parser.rosmsg "uint8 data"
    > render msg
    "uint8 data"

### QuasiQuotes

    > [Robotics.ROS.Msg.TH.rosmsgFrom|/opt/ros/indigo/share/std_msgs/msg/Byte.msg|]
    "[Variable (\"data\",Simple RByte)]"


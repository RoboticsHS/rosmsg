# ROS message parser / render / .hs generator

[![Build Status](https://travis-ci.org/RoboticsHS/rosmsg.svg?branch=master)](https://travis-ci.org/RoboticsHS/rosmsg)

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


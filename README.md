# ROS message parser / render / .hs generator

    $ git clone https://github.com/RoboticsHS/rosmsg
    $ cd rosmsg
    $ stack ghci

    > let Done _ msg = parse Robotics.ROS.Msg.Parser.rosmsg "uint8 data"
    > render msg
    "uint8 data"

    > [Robotics.ROS.Msg.TH.rosmsgFrom|/opt/ros/indigo/share/std_msgs/msg/Byte.msg|]
    "[Variable (\"data\",Simple RByte)]"


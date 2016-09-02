module Robotics.ROS.Msg.Render (render) where

import Data.Text.Lazy.Builder (Builder, fromText, fromString)
import Data.Char (toLower)
import Data.Monoid ((<>))

import Robotics.ROS.Msg.Types

isConstant :: FieldDefinition -> Bool
isConstant (Constant _ _) = True
isConstant _ = False

rosType :: FieldType -> Builder
rosType (Simple t) = fromString $ fmap toLower $ drop 1 $ show t 
rosType (Custom t) = fromString $ show t
rosType (Array t)  = rosType t <> "[]"
rosType (FixedArray l t) = rosType t <> "[" <> fromString (show l) <> "]"

-- |Render ROS message definition according to
-- * comments removed
-- * whitespace removed
-- * package names of dependencies removed
-- * constants reordered ahead of other declarations
--   from http://www.ros.org/wiki/ROS/Technical%20Overview
render :: MsgDefinition -> Builder
render = foldl1 (\a b -> a <> "\n" <> b) . fmap go . sort
  where sort v = filter isConstant v ++ filter (not . isConstant) v
        go (Constant (name, typ) val) = rosType typ <> " " <>
                                        fromText name <> "=" <> fromText val
        go (Variable (name, typ)) = rosType typ <> " " <> fromText name

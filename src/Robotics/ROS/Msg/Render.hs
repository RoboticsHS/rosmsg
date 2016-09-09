module Robotics.ROS.Msg.Render
  ( render
  , render'
  ) where

import Data.Text.Lazy.Builder (Builder, fromText, fromString)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Text (Text)

import Robotics.ROS.Msg.Types

isConstant :: FieldDefinition -> Bool
isConstant (Constant _ _) = True
isConstant _ = False

rosType :: (Text -> Text) -> FieldType -> Builder
rosType _ (Simple t) = fromString $ fmap toLower $ drop 1 $ show t 
rosType u (Custom t) = fromText $ u t
rosType u (Array t)        = rosType u t <> "[]"
rosType u (FixedArray l t) = rosType u t <> "[" <> fromString (show l) <> "]"

-- |Render ROS message definition according to
-- * comments removed
-- * whitespace removed
-- * package names of dependencies removed
-- * constants reordered ahead of other declarations
--   from http://www.ros.org/wiki/ROS/Technical%20Overview
render' :: (Text -> Text) -> MsgDefinition -> Builder
render' uType = foldl (\a b -> a <> "\n" <> b) mempty . fmap go . sort
  where sort v = filter isConstant v ++ filter (not . isConstant) v
        go (Constant (typ, name) val) = rosType uType typ <> " " <>
                                        fromText name <> "=" <> fromText val
        go (Variable (typ, name)) = rosType uType typ <> " " <> fromText name

render :: MsgDefinition -> Builder
render = render' id

-- |
-- Module      :  Robotics.ROS.Msg.Render
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- The ROS message language builder from abstract message definition.
--
module Robotics.ROS.Msg.Render (
  -- * Lazy Text builder
    render
  -- * Stric Text render
  , renderT
  ) where

import Data.Text.Lazy.Builder (Builder, fromText,
                               fromString, toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Text (Text)

import Robotics.ROS.Msg.Types

-- | Builder creator from 'FieldType'
rosType :: FieldType -> Builder
rosType (Simple t)       = fromString $ fmap toLower $ drop 1 $ show t
rosType (Custom t)       = fromText t
rosType (Array t)        = rosType t <> "[]"
rosType (FixedArray l t) = rosType t <> "[" <> fromString (show l) <> "]"

-- | Create lazy text builder from message definition
--
-- Render formal ROS message definition according to:
--
--     * comments removed
--
--     * whitespace removed
--
--     * package names of dependencies removed
--
--     * constants reordered ahead of other declarations
--       from http://www.ros.org/wiki/ROS/Technical%20Overview
--
-- The @genmsg@ python implementation says:
-- Compute the text used for md5 calculation. MD5 spec states that we
-- removes comments and non-meaningful whitespace. We also strip
-- packages names from type names. For convenience sake, constants are
-- reordered ahead of other declarations, in the order that they were
-- originally defined.
--
render :: MsgDefinition -> Builder
render []     = ""
render msgdef = unlines1 (go <$> specOrder msgdef)
  where specOrder v = filter isConstant v ++ filter (not . isConstant) v
        unlines1    = foldl1 (\a b -> a <> "\n" <> b)

        go (Constant (typ, name) val) = rosType typ <> " " <>
                                        fromText name <> "=" <> fromText val
        go (Variable (typ, name)) = rosType typ <> " " <> fromText name

        isConstant x = case x of
            Constant _ _ -> True
            _ -> False

-- | Render message definition to 'Text' instantly
renderT :: MsgDefinition -> Text
renderT = toStrict . toLazyText . render

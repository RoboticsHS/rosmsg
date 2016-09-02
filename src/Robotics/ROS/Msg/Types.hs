module Robotics.ROS.Msg.Types
  ( FieldDefinition(..)
  , SimpleType(..)
  , FieldType(..)
  , MsgDefinition
  , ROSDuration
  , FieldName
  , ROSTime
  , Field
  ) where

import Data.Word (Word32)
import Data.Text (Text)

-- |A variant type describing the simple types 
-- that may be included in a ROS message.
data SimpleType
  = RBool
  | RByte
  | RChar
  | RInt8
  | RUInt8
  | RInt16
  | RUInt16
  | RInt32
  | RUInt32
  | RInt64
  | RUInt64
  | RFloat32
  | RFloat64
  | RString
  | RTime
  | RDuration
  deriving (Show, Enum, Eq)

-- |A variant type describing the types that may be included in a ROS
-- message.
data FieldType
  = Simple SimpleType
  | Custom Text
  | Array FieldType
  | FixedArray Int FieldType
  deriving (Show, Eq)

-- |Field name is text encoded
type FieldName = Text

-- |Field is a pair of name - value
type Field = (FieldType, FieldName)

-- |ROS message field is a variable or constant declaration
data FieldDefinition
  = Variable Field
  -- ^ Variable field name and type
  | Constant Field Text 
  -- ^ Constant field name, type and value
  deriving (Show, Eq)

-- |ROS message is a list of fields
type MsgDefinition = [FieldDefinition]

-- |ROSDuration is a tuple of (seconds, nanoseconds)
type ROSDuration = (Word32, Word32)

-- |ROSTime is a tuple of (seconds, nanoseconds)
type ROSTime = (Word32, Word32)

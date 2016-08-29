module Robotics.ROS.Msg.Types
  ( MsgDefinition(..)
  , SimpleType(..)
  , FieldType(..)
  , MsgField(..)
  , ROSDuration
  , ROSTime
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Data.Digest.Pure.MD5 (MD5Digest)

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
  deriving (Show, Enum, Eq, Ord)

-- |A variant type describing the types that may be included in a ROS
-- message.
data FieldType
  = Simple SimpleType
  | Custom Text
  | Array FieldType
  | FixedArray Int FieldType
  deriving (Show, Eq, Ord)

-- |Message field type
data MsgField
  = Variable Text FieldType
  -- ^ Variable field name and type
  | Constant Text SimpleType Text 
  -- ^ Constant field name, type and value
  deriving (Show, Eq, Ord)

-- |A message definition has a name, type, package and a list of
-- named, typed fields.
data MsgDefinition = MsgDefinition
  { msgName    :: Text
  , msgPackage :: Text
  , msgSource  :: Text
  , msgFields  :: [MsgField]
  } deriving (Show, Eq, Ord)

-- |ROSDuration is a tuple of (seconds, nanoseconds)
type ROSDuration = (Word32, Word32)

-- |ROSTime is a tuple of (seconds, nanoseconds)
type ROSTime = (Word32, Word32)

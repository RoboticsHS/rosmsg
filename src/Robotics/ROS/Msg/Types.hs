module Robotics.ROS.Msg.Types
  ( MsgDefinition(..)
  , FieldType(..)
  , MsgField(..)
  , MsgMeta(..)
  , ROSDuration
  , ROSTime
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Data.Digest.Pure.MD5 (MD5Digest)

-- |A variant type describing the types that may be included in a ROS
-- message.
data FieldType
  = F_Bool
  | F_Byte
  | F_Char
  | F_Int8
  | F_UInt8
  | F_Int16
  | F_UInt16
  | F_Int32
  | F_UInt32
  | F_Int64
  | F_UInt64
  | F_Float32
  | F_Float64
  | F_String
  | F_Time
  | F_Duration
  | F_Array FieldType
  | F_FixedArray Int FieldType
  | F_UserType Text
  deriving (Show, Eq, Ord, Enum)

-- |Field name and type
data MsgField = Variable Text FieldType
              | Constant Text FieldType 
  deriving (Show, Eq, Ord)

-- | A Haskell type for a message definition.
data MsgMeta = MsgMeta
    { msgName     :: Text
    , msgTypeName :: Text
    , msgPackage  :: Text
    } deriving (Show, Eq, Ord)

-- |A message definition has a name, an md5 sum, and a list of
-- named, typed fields.
data MsgDefinition = MsgDefinition
  { msgMeta   :: MsgMeta
  , msgMD5    :: MD5Digest
  , msgFields :: [MsgField]
  } deriving (Show, Eq, Ord)

-- |ROSDuration is a tuple of (seconds, nanoseconds)
type ROSDuration = (Word32, Word32)

-- |ROSTime is a tuple of (seconds, nanoseconds)
type ROSTime = (Word32, Word32)

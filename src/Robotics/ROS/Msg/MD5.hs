-- |
-- Module      :  Robotics.ROS.Msg.MD5
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  exp
-- Portability :  POSIX / WIN32
--
-- ROS message MD5 recurrent hash. Replace user types by it's MD5 and
-- take hash of result.
--
module Robotics.ROS.Msg.MD5 (
    MD5Digest
  , computeMD5
  ) where

import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text, replace, pack)

-- | Compute MD5 for given message with user type hashes
--
-- Recursively generate MD5 for subtype. Have to build up 
-- dependency representation for subtype in order to                                     
-- generate MD5. 
computeMD5 :: [(Text, MD5Digest)] -- ^ List of pairs: user type name, message hash
           -> Text                -- ^ Full message source
           -> MD5Digest           -- ^ Message MD5
computeMD5 [] msg = md5 (fromStrict (encodeUtf8 msg))
computeMD5 ((dep, hash) : xs) msg = computeMD5 xs msg'
  where msg' = replace dep (pack $ show hash) msg

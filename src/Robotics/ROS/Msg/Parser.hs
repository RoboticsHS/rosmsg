-- |
-- Module      :  Robotics.ROS.Msg.Parser
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Parser components for the ROS message description language (@msg@
-- files). See http://wiki.ros.org/msg for reference.
--
module Robotics.ROS.Msg.Parser (
  -- * Attoparsec re-export
    Result(..)
  , parse
  -- * The ROS message language parser
  , rosmsg
  ) where

import           Data.Text (Text, pack, toLower)
import           Data.Char (isDigit, isAlpha)
import           Data.Attoparsec.Text.Lazy
import           Control.Arrow ((&&&))
import           Data.Either (rights)
import qualified Data.Text as T

import           Robotics.ROS.Msg.Types

-- | Show simple type
showType :: Show a => a -> Text
showType = toLower . T.drop 1 . pack . show

-- | All of simple type and its text representation list
simpleAssoc :: [(SimpleType, Text)]
simpleAssoc = (id &&& showType) <$> enumFrom RBool

-- | Line getter
takeLine :: Parser Text
takeLine = pack <$> manyTill anyChar (eitherP endOfLine endOfInput)

-- | Valid ROS identifier parser
identifier :: Parser Text
identifier = takeWhile1 validChar
  where validChar c = any ($ c) [isDigit, isAlpha, (== '_'), (== '/')]

-- | ROS message comments parser
comment :: Parser ()
comment = skipSpace *> char '#' *> takeLine *> pure ()

-- | Parse fields defined in the message
variableParser :: Parser FieldDefinition
variableParser = do
    typeIdent <- choice [simpleField, customField] 
    mkField   <- choice [flat, array, fixedArray]
    return (Variable $ mkField typeIdent)
  where
    -- Build-in type parser
    simpleField = Simple . fst <$> choice (mapM string <$> simpleAssoc)

    -- User type parser
    customField = Custom . dropPkgSpec <$> identifier

    -- Drop package spec from user type
    dropPkgSpec = last . T.split (== '/')

    -- Flat type is no array
    flat = do
        name <- space *> skipSpace *> identifier <* takeLine
        return $ flip (,) name

    -- Variable lenght array
    array = do
        name <- skipSpace *> string "[]" *> skipSpace *> identifier <* takeLine
        return $ flip (,) name . Array

    -- Fixed lenght array
    fixedArray = do
        len <- skipSpace *> char '[' *> decimal <* char ']'
        name <- skipSpace *> identifier <* takeLine
        return $ flip (,) name . FixedArray len

-- | Parse constants defined in the message
constantParser :: Parser FieldDefinition
constantParser = choice (go <$> enumFrom RBool)
  where
    go t = do
        name <- string (showType t) *> skipSpace *> identifier <* space
        value <- skipSpace *> char '=' *> skipSpace *> takeLine
        return $ Constant (Simple t, name) $
            -- String constants are parsed somewhat differently from numeric
            -- constants. For numerical constants, we drop comments and trailing
            -- spaces. For strings, we take the whole line (so comments aren't
            -- stripped).
            case t of
                RString -> value
                _       -> T.takeWhile (/= '#') value

-- | The ROS message language parser
rosmsg :: Parser MsgDefinition
rosmsg = rights <$> many' (eitherP junk field)
  where field = choice [constantParser, variableParser]
        junk  = choice [comment, endOfLine]

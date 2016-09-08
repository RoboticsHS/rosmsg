-- |Parser components for the ROS message description language (@msg@
-- files). See http://wiki.ros.org/msg for reference.
module Robotics.ROS.Msg.Parser
  ( Result(..)
  , parse
  , rosmsg
  ) where

import Data.Text (Text, pack, toLower)
import Data.Char (isDigit, isAlpha)
import Data.Attoparsec.Text.Lazy
import qualified Data.Text as T
import Control.Arrow ((&&&))
import Data.Either (rights)

import Robotics.ROS.Msg.Types

-- |Show simple type
showType :: Show a => a -> Text
showType = toLower . T.drop 1 . pack . show

-- |All of simple type and its text representation list
simpleAssoc :: [(SimpleType, Text)]
simpleAssoc = (id &&& showType) <$> enumFrom RBool

-- |Line getter
takeLine :: Parser Text
takeLine = pack <$> manyTill anyChar (eitherP endOfLine endOfInput)

-- |Valid ROS identifier parser
identifier :: Parser Text
identifier = takeWhile1 validChar
  where validChar c = any ($ c) [isDigit, isAlpha, (== '_'), (== '/')]

-- |ROS message comments parser
comment :: Parser ()
comment = skipSpace *> char '#' *> takeLine *> pure ()

-- |Parse fields defined in the message
variableParser :: Parser FieldDefinition
variableParser = do
    typeIdent <- choice [simpleField, customField] 
    mkField   <- choice [plain, array, fixedArray]
    return (Variable $ mkField typeIdent)
  where
    simpleField = Simple . fst <$> choice (mapM string <$> simpleAssoc)

    customField = Custom <$> identifier

    plain = do
        name <- space *> skipSpace *> identifier <* takeLine
        return $ flip (,) name

    array = do
        name <- skipSpace *> string "[]" *> skipSpace *> identifier <* takeLine
        return $ flip (,) name . Array

    fixedArray = do
        len <- skipSpace *> char '[' *> decimal <* char ']'
        name <- skipSpace *> identifier <* takeLine
        return $ flip (,) name . FixedArray len

-- |Parse constants defined in the message
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

-- |ROS message parser
rosmsg :: Parser MsgDefinition
rosmsg = rights <$> many' (eitherP junk field)
  where field = choice [constantParser, variableParser]
        junk  = choice [comment, endOfLine]

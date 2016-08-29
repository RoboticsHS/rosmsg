{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- |Parser components for the ROS message description language (@msg@
-- files). See http://wiki.ros.org/msg for reference.
module Robotics.ROS.Msg.Parser (msgParser, parse) where

import qualified Data.Text as T
import Data.Text (Text, pack, toLower)
import Data.Char (isDigit, isAlpha, isSpace)
import Data.Scientific (toBoundedInteger)
import Data.Attoparsec.Text.Lazy
import Data.Foldable (foldl')
import Control.Arrow ((&&&))
import Data.Either (rights)

import Robotics.ROS.Msg.Types

type FieldName = Text

showType :: Show a => a -> Text
showType = toLower . T.drop 1 . pack . show

simpleAssoc :: [(SimpleType, Text)]
simpleAssoc = (id &&& showType) <$> enumFrom RBool

takeLine :: Parser Text
takeLine = pack <$> manyTill anyChar (eitherP endOfLine endOfInput)

identifier :: Parser Text
identifier = takeWhile1 validChar
  where validChar c = any ($ c) [isDigit, isAlpha, (== '_'), (== '/')]

comment :: Parser ()
comment = skipSpace *> char '#' *> takeLine *> pure ()

-- |Parse fields defined in the message
fieldParser :: Parser MsgField
fieldParser = do
    typeIdent <- choice [simpleField, customField] 
    mkField   <- choice [plain, array, fixedArray]
    return $ uncurry Variable (mkField typeIdent)
  where
    simpleField = Simple . fst <$> choice (mapM string <$> simpleAssoc)

    customField = Custom <$> identifier

    plain = do
        name <- space *> skipSpace *> identifier <* takeLine
        return $ (name,)

    array = do
        name <- skipSpace *> string "[]" *> skipSpace *> identifier <* takeLine
        return $ (name,) . Array

    fixedArray = do
        len <- skipSpace *> char '[' *> decimal <* char ']'
        name <- skipSpace *> identifier <* takeLine
        return $ (name,) . FixedArray len 

-- |Parse constants defined in the message
constParser :: Parser MsgField
constParser = choice (go <$> enumFrom RBool)
  where
    go t = do
        name <- string (showType t) *> skipSpace *> identifier <* space
        value <- skipSpace *> char '=' *> skipSpace *> takeLine
        return (Constant name t value) 

-- String constants are parsed somewhat differently from numeric
-- constants. For numerical constants, we drop comments and trailing
-- spaces. For strings, we take the whole line (so comments aren't
-- stripped).
sanitizeField :: MsgField -> MsgField
sanitizeField field = case field of
    Variable name t -> Variable (sanitize name) t
    Constant name RString v -> Constant (sanitize name) RString v
    Constant name t val     -> Constant (sanitize name) t $
        T.takeWhile (\c -> c /= '#' && not (isSpace c)) val

-- |Ensure that field and constant names are valid Haskell identifiers
-- and do not coincide with Haskell reserved words.
sanitize :: Text -> Text
sanitize "data"    = "_data"
sanitize "type"    = "_type"
sanitize "class"   = "_class"
sanitize "module"  = "_module"
sanitize "newtype" = "_newtype"
sanitize x = toLower x 

-- |ROS message fields parser
msgParser :: Parser [MsgField]
msgParser = do
    fields <- many1 $ eitherP thrash field
    return (sanitizeField <$> rights fields)
  where field  = choice [constParser, fieldParser]
        thrash = choice [comment, endOfLine]

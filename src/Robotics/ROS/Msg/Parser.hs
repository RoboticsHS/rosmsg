{-# LANGUAGE OverloadedStrings #-}
-- |Parser components for the ROS message description language (@msg@
-- files). See http://wiki.ros.org/msg for reference.
module Robotics.ROS.Msg.Parser (parseMsg, parseSrv, simpleFieldAssoc) where

import Prelude hiding (takeWhile, drop)
import Control.Arrow ((&&&))
import Data.Attoparsec.Text.Lazy
import qualified Data.Text as T
import Data.Text

import System.FilePath (dropExtension, takeFileName, splitDirectories)

import Robotics.ROS.Msg.Types

simpleFieldAssoc :: [(FieldType, Text)]
simpleFieldAssoc = fmap (id &&& fieldTypeName) fieldTypes 
  where fieldTypes = enumFormTo F_Bool F_Duration
        fieldTypeName = toLower . drop 2 . pack . show) 

eatLine :: Parser ()
eatLine = manyTill anyChar (eitherP endOfLine endOfInput) *> skipSpace

parseName :: Parser Text
parseName = skipSpace *> identifier <* eatLine <* try comment

identifier :: Parser Text
identifier = B.cons <$> letter_ascii <*> takeWhile validChar
    where validChar c = any ($ c) [isDigit, isAlpha_ascii, (== '_'), (== '/')]

parseInt :: Parser Int
parseInt = foldl' (\s x -> s*10 + digitToInt x) 0 <$> many1 digit

comment :: Parser [()]
comment = many $ skipSpace *> try (char '#' *> eatLine)

simpleParser :: (MsgType, ByteString) -> Parser (ByteString, MsgType)
simpleParser (t,b) = (, t) <$> (string b *> space *> parseName)

fixedArrayParser :: (MsgType, ByteString) -> Parser (ByteString, MsgType)
fixedArrayParser (t,b) = (\len name -> (name, RFixedArray len t)) <$>
                         (string b *> char '[' *> parseInt <* char ']') <*> 
                         (space *> parseName)

varArrayParser :: (MsgType, ByteString) -> Parser (ByteString, MsgType)
varArrayParser (t,b) = (, RVarArray t) <$> 
                       (string b *> string "[]" *> space *> parseName)

userTypeParser :: Parser (ByteString, MsgType)
userTypeParser = choice [userSimple, userVarArray, userFixedArray]

userSimple :: Parser (ByteString, MsgType)
userSimple = (\t name -> (name, RUserType t)) <$>
             identifier <*> (space *> parseName)

userVarArray :: Parser (ByteString, MsgType)
userVarArray = (\t name -> (name, RVarArray (RUserType t))) <$>
               identifier <*> (string "[]" *> space *> parseName)

userFixedArray :: Parser (ByteString, MsgType)
userFixedArray = (\t n name -> (name, RFixedArray n (RUserType t))) <$>
                 identifier <*> 
                 (char '[' *> parseInt <* char ']') <*> 
                 (space *> parseName)

-- Parse constants defined in the message
constParser :: ByteString -> MsgType -> 
               Parser (ByteString, MsgType, ByteString)
constParser s x = (,x,) <$> 
                  (string s *> space *> identifier) <*> 
                  (skipSpace *> char '=' *> skipSpace *> restOfLine <* skipSpace)
    where restOfLine :: Parser ByteString
          restOfLine = pack <$> manyTill anyChar (eitherP endOfLine endOfInput)

constParsers :: [Parser (ByteString, MsgType, ByteString)]
constParsers = map (uncurry constParser . swap) simpleFieldAssoc
  where swap (x,y) = (y,x)

-- String constants are parsed somewhat differently from numeric
-- constants. For numerical constants, we drop comments and trailing
-- spaces. For strings, we take the whole line (so comments aren't
-- stripped).
sanitizeConstants :: (a, MsgType, ByteString) -> (a, MsgType, ByteString)
sanitizeConstants c@(_, RString, _) = c
sanitizeConstants (name, t, val) = 
    (name, t, B.takeWhile (\c -> c /= '#' && not (isSpace c)) val)

-- Parsers fields and constants.
fieldParsers :: [Parser (Either (ByteString, MsgType) 
                                (ByteString, MsgType, ByteString))]
fieldParsers = map (comment *>) $
               map (Right . sanitizeConstants <$>) constParsers ++ 
               map (Left <$>) (builtIns ++ [userTypeParser])
    where builtIns = concatMap (`map` simpleFieldAssoc)
                               [simpleParser, fixedArrayParser, varArrayParser]

mkParser :: MsgName -> String -> ByteString -> Parser Msg
mkParser sname lname txt = aux . partitionEithers <$> many (choice fieldParsers)
  where aux (fs, cs) = Msg sname lname txt
                           (map buildField fs)
                           (map buildConst cs)

buildField :: (ByteString, MsgType) -> MsgField
buildField (name,typ) = MsgField fname typ name
  where fname = B.append "_" $ sanitize name

buildConst :: (ByteString, MsgType, ByteString) -> MsgConst
buildConst (name,typ,val) = MsgConst fname typ val name
  where fname = B.map toLower $ sanitize name

-- |Ensure that field and constant names are valid Haskell identifiers
-- and do not coincide with Haskell reserved words.
sanitize :: ByteString -> ByteString
sanitize "data" = "_data"
sanitize "type" = "_type"
sanitize "class" = "_class"
sanitize "module" = "_module"
sanitize x = B.cons (toLower (B.head x)) (B.tail x)

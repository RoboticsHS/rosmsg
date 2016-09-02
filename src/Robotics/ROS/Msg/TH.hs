{-# LANGUAGE TemplateHaskell, QuasiQuotes, RankNTypes, KindSignatures #-}
module Robotics.ROS.Msg.TH
  ( rosmsg
  , rosmsgFrom
  ) where

import Data.Attoparsec.Text.Lazy (Result(..))
import Data.Text.Lazy.Builder (toLazyText)
import Data.Char (isAlphaNum, toLower)
import Data.Text.Lazy (pack, unpack)
import qualified Data.Text as T
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Maybe (catMaybes)
import Data.List (groupBy)

import qualified Robotics.ROS.Msg.Parser as P
import Robotics.ROS.Msg.Render
import Robotics.ROS.Msg.Types

rosmsgFrom :: QuasiQuoter
rosmsgFrom = quoteFile rosmsg

rosmsg :: QuasiQuoter
rosmsg = QuasiQuoter
  { quoteDec  = quoteMsgDec
  , quoteExp  = quoteMsgExp
  , quotePat  = undefined
  , quoteType = undefined
  }

typeQ :: FieldType -> TypeQ
typeQ (Simple t)       = conT $ mkName $ mkFlatType t
typeQ (Custom t)       = conT $ mkName $ T.unpack t
typeQ (Array t)        = appT (conT $ mkName "ROSArray") (typeQ t)
typeQ (FixedArray l t) = appT (appT (conT $ mkName "ROSFixedArray") (typeQ t))
                              (litT $ numTyLit $ fromIntegral l)

-- Ensure that field and constant names are valid Haskell identifiers
-- and do not coincide with Haskell reserved words.
sanitizeField :: FieldDefinition -> FieldDefinition
sanitizeField = \case
    Constant (a, b) c -> Constant (sanitize a, b) c
    Variable (a, b)   -> Variable (sanitize a, b) 
  where sanitize x | isKeyword x = T.cons '_' x
                   | otherwise   = x
        isKeyword = flip elem [ "as", "case", "of", "class"
                              , "data", "family", "instance"
                              , "default", "deriving", "do"
                              , "forall", "foreign", "hiding"
                              , "if", "then", "else", "import"
                              , "infix", "infixl", "infixr"
                              , "let", "in", "mdo", "module"
                              , "newtype", "proc", "qualified"
                              , "rec", "type", "where"]

-- Generate the name of the Haskell type that corresponds to a flat
-- (i.e. non-array) ROS type.
mkFlatType :: SimpleType -> String
mkFlatType = \case
    RBool     -> "Bool"
    RInt8     -> "Int8"
    RUInt8    -> "Word8"
    RByte     -> "Word8"
    RChar     -> "Int8"
    RInt16    -> "Int16"
    RUInt16   -> "Word16"
    RInt32    -> "Int32"
    RUInt32   -> "Word32"
    RInt64    -> "Int64"
    RUInt64   -> "Word64"
    RFloat32  -> "Float"
    RFloat64  -> "Double"
    RString   -> "ByteString"
    RDuration -> "ROSDuration"
    RTime     -> "ROSTime"

fieldQ :: FieldDefinition -> Maybe VarStrictTypeQ
fieldQ (Constant _ _)    = Nothing 
fieldQ (Variable (name, typ)) = Just $ varStrictType recName recType
  where recName = mkName ('_' : T.unpack name)
        recType = strictType notStrict (typeQ typ)

-- Generate the getDigest Message class implementation 
getDigest :: MsgDefinition -> DecQ
getDigest msg = funD (mkName "getDigest") [digest]
  where
    digest = clause [wildP] (normalB (appE md5 (stringE src))) []
    src    = unpack $ toLazyText $ render msg
    md5    = varE (mkName "md5")

-- Generate the getType Message class implementation 
getType :: String -> DecQ
getType t = funD (mkName "getType") [typeString]
  where
    typeString = clause [wildP] (normalB (stringE msgType)) []
    msgType = let [m, p] = fmap (drop 1) $ take 2 $
                           reverse $ groupBy (const (/= '.')) t
              in fmap toLower p ++ "/" ++ m

-- Generate def method of Default instance
defFun :: Name -> [a] -> DecQ
defFun name recs = funD defName [def] 
  where def = clause [] (normalB (foldl appE (conE name) recsDef)) []
        defName = mkName "def"
        recsDef = (const (varE defName)) <$> recs

-- Lens signature
lensSig :: Name -> TypeQ -> TypeQ -> DecQ
lensSig lensName a b =
    sigD lensName [t|forall f. Functor f => ($b -> f $b) -> $a -> f $a|]

-- Given a record field name,
-- produces a single function declaration:
-- lensName :: forall f. Functor f => (a -> f a') -> b -> f b' 
-- lensName f a = (\x -> a { field = x }) `fmap` f (field a)
-- FROM: Lens.Family.THCore
deriveLens :: Name -> FieldDefinition -> [DecQ]
deriveLens _ (Constant _ _)       = []
deriveLens dataName (Variable (name, typ)) = [ lensSig lensName (conT dataName) (typeQ typ)
                                             , funD lensName [defLine]]
  where a = mkName "a"
        f = mkName "f"
        lensName  = mkName (T.unpack name) 
        fieldName = mkName ('_' : T.unpack name) 
        defLine   = clause pats (normalB body) []
        pats = [varP f, varP a]
        body = [| (\x -> $(record a fieldName [|x|]))
                  <$> $(appE (varE f) (appE (varE fieldName) (varE a)))
                |]
        record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

instanceD' :: Name -> TypeQ -> [DecQ] -> DecQ
instanceD' name insType insDecs =
    instanceD (cxt []) (appT insType (conT name)) insDecs

quoteMsgDec :: String -> Q [Dec]
quoteMsgDec txt = do
    l_mod <- loc_module <$> location
    let dataName    = mkDataName l_mod
        fields      = sanitizeField <$> msg
        recordList  = catMaybes (fieldQ <$> fields)
        dataRecords = recC dataName recordList

    lenses      <- sequence (concat (deriveLens dataName <$> fields))

    msgData     <- dataD (cxt []) dataName [] [dataRecords] derivingD
    binInstance <- instanceD' dataName binaryC []
    defInstance <- instanceD' dataName defaultC [defFun dataName recordList] 
    msgInstance <- instanceD' dataName messageC [getDigest msg, getType l_mod]

    return $ [ msgData
             , binInstance
             , defInstance
             , msgInstance
             ] ++ lenses 
  where Done _ msg = P.parse P.rosmsg (pack txt)
        mkDataName = mkName . drop 1 . last . groupBy (const isAlphaNum)
        binaryC    = conT (mkName "Binary")
        defaultC   = conT (mkName "Default")
        messageC   = conT (mkName "Message")
        derivingD  = [ mkName "Show", mkName "Eq", mkName "Ord"
                     , mkName "Generic", mkName "Data", mkName "Typeable"
                     ]

quoteMsgExp :: String -> ExpQ
quoteMsgExp txt = stringE (show msg)
  where Done _ msg = P.parse P.rosmsg (pack txt)
{-# LANGUAGE TemplateHaskell, QuasiQuotes, RankNTypes, KindSignatures #-}
module Robotics.ROS.Msg.TH
  ( rosmsg
  , rosmsgFrom
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Attoparsec.Text.Lazy (Result(..))
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Char (isAlphaNum, toLower)
import           Data.Text.Lazy (pack, unpack)
import           Data.Digest.Pure.MD5 (md5)
import           Data.Maybe (catMaybes)
import           Text.Printf (printf)
import           Data.List (groupBy)
import qualified Data.Text as T

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH

import qualified Robotics.ROS.Msg.Parser as P
import           Robotics.ROS.Msg.Render (render')
import           Robotics.ROS.Msg.Types
import           Robotics.ROS.Msg

-- |Generate ROS message declarations from file
rosmsgFrom :: QuasiQuoter
rosmsgFrom = quoteFile rosmsg

-- |ROS message QQ for data type and instances generation
rosmsg :: QuasiQuoter
rosmsg = QuasiQuoter
  { quoteDec  = quoteMsgDec
  , quoteExp  = quoteMsgExp
  , quotePat  = undefined
  , quoteType = undefined
  }

-- Take list of external types used in message
externalTypes :: MsgDefinition -> [TypeQ]
externalTypes msg =
    conT . mkName . T.unpack <$> catMaybes (go <$> msg)
  where
    go (Variable (Custom t, _))                = Just t
    go (Variable (Array (Custom t), _))        = Just t
    go (Variable (FixedArray _ (Custom t), _)) = Just t
    go _ = Nothing

-- Field to Type converter
typeQ :: FieldType -> TypeQ
typeQ (Simple t)       = conT $ mkName $ mkFlatType t
typeQ (Custom t)       = conT $ mkName $ T.unpack t
typeQ (Array t)        = appT (conT $ mkName "ROSArray") (typeQ t)
typeQ (FixedArray l t) = appT (appT (conT $ mkName "ROSFixedArray") (typeQ t))
                              (litT $ numTyLit $ fromIntegral l)

-- Ensure that field and constant names are valid Haskell identifiers
-- and do not coincide with Haskell reserved words.
sanitizeField :: FieldDefinition -> FieldDefinition
sanitizeField (Constant (a, b) c) = Constant (a, sanitize b) c
sanitizeField (Variable (a, b))   = Variable (a, sanitize b) 

-- Sanitize identifier for valid Haskell
sanitize :: T.Text -> T.Text
sanitize x | isKeyword x = T.cons '_' x
           | otherwise   = x
  where isKeyword = flip elem [ "as", "case", "of", "class"
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
mkFlatType t = case t of
    RBool     -> "P.Bool"
    RInt8     -> "I.Int8"
    RUInt8    -> "W.Word8"
    RByte     -> "W.Word8"
    RChar     -> "I.Int8"
    RInt16    -> "I.Int16"
    RUInt16   -> "W.Word16"
    RInt32    -> "I.Int32"
    RUInt32   -> "W.Word32"
    RInt64    -> "I.Int64"
    RUInt64   -> "W.Word64"
    RFloat32  -> "P.Float"
    RFloat64  -> "P.Double"
    RString   -> "BS.ByteString"
    RDuration -> "ROSDuration"
    RTime     -> "ROSTime"

-- Field definition to record var converter
fieldQ :: FieldDefinition -> Maybe VarStrictTypeQ
fieldQ (Constant _ _)         = Nothing 
fieldQ (Variable (typ, name)) = Just $ varStrictType recName recType
  where recName = mkName ('_' : T.unpack name)
        recType = strictType notStrict (typeQ typ)

-- Generate the getDigest Message class implementation 
mkGetDigest :: MsgDefinition -> DecQ
mkGetDigest msg = funD (mkName "getDigest") [digest]
  where
    digest      = clause [wildP] (normalB digestE) []
    digestE     = [| md5 (LBS.pack $(source)) |]
    source      = appsE $ [[|printf|], stringE src] ++ deps
    src         = unpack $ toLazyText $ render' (const "%s") msg
    deps        = depDigest <$> externalTypes msg
    depDigest t = [|show (getDigest (undefined :: $(t)))|]

-- Generate the getType Message class implementation 
mkGetType :: DecQ
mkGetType = do
    l_mod <- loc_module <$> location
    let typeString = clause [wildP] (normalB (stringE msgType)) []
        msgType = let [m, p] = fmap (drop 1) $ take 2 $
                           reverse $ groupBy (const (/= '.')) l_mod
                   in fmap toLower p ++ "/" ++ m
    funD (mkName "getType") [typeString]

-- Generate def method of Default instance
mkDef :: Name -> [a] -> DecQ
mkDef name recs = funD defName [def] 
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
deriveLens _ (Constant _ _) = []
deriveLens dataName (Variable (typ, name)) =
    [ lensSig lensName (conT dataName) (typeQ typ)
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

-- |Instance declaration with empty context
instanceD' :: Name -> TypeQ -> [DecQ] -> DecQ
instanceD' name insType insDecs =
    instanceD (cxt []) (appT insType (conT name)) insDecs

-- |Lenses declarations
mkLenses :: Name -> MsgDefinition -> [DecQ]
mkLenses name msg =
    concat (deriveLens name <$> fields)
  where
    fields     = sanitizeField <$> msg

-- |Data type declaration
mkData :: Name -> MsgDefinition -> [DecQ]
mkData name msg = pure $
    dataD (cxt []) name [] [recs] derivingD
  where
    fields     = sanitizeField <$> msg
    recs       = recC name (catMaybes (fieldQ <$> fields))
    derivingD  = [ mkName "P.Show", mkName "P.Eq", mkName "P.Ord"
                 , mkName "Generic", mkName "Data", mkName "Typeable"
                 ]

-- |Binary instance declaration
mkBinary :: Name -> a -> [DecQ]
mkBinary name _ = pure $
    instanceD' name binaryT []
  where
    binaryT    = conT (mkName "Binary")

-- |Default instance declaration
mkDefault :: Name -> MsgDefinition -> [DecQ]
mkDefault name msg = pure $
    instanceD' name defaultT [mkDef name recordList]
  where
    recordList = catMaybes (fieldQ <$> fields)
    defaultT   = conT (mkName "Default")
    fields     = sanitizeField <$> msg

-- |Message instance declaration
mkMessage :: Name -> MsgDefinition -> [DecQ]
mkMessage name msg = pure $
    instanceD' name messageT [mkGetDigest msg, mkGetType]
  where
    messageT = conT (mkName "Message")

-- |Stamped instance declaration
mkStamped :: Name -> MsgDefinition -> [DecQ]
mkStamped name msg | hasHeader msg = pure go
                   | otherwise = []
  where
    hasHeader [Variable (Custom "Header", _), _] = True
    hasHeader _ = False

    lensE f    = [|$(dyn "header") . $(dyn f)|]
    seqLensE   = lensE "seq"
    stampLensE = lensE "stamp"
    frameLensE = lensE "frame_id"

    mkGetSequence = funD (mkName "getSequence") [
        clause [] (normalB (appE (dyn "view") seqLensE)) []]

    mkSetSequence = funD (mkName "setSequence") [
        clause [] (normalB (appE (dyn "set") seqLensE)) []]

    mkGetStamp = funD (mkName "getStamp") [
        clause [] (normalB (appE (dyn "view") stampLensE)) []]

    mkGetFrame = funD (mkName "getFrame") [
        clause [] (normalB (appE (dyn "view") frameLensE)) []]

    stampedT = conT (mkName "Stamped")

    go = instanceD' name stampedT [ mkGetSequence
                                  , mkSetSequence
                                  , mkGetStamp
                                  , mkGetFrame ]

quoteMsgDec :: String -> Q [Dec]
quoteMsgDec txt = do
    name <- mkDataName . loc_module <$> location
    sequence $ concatMap (msgRun name) $
      [ mkData
      , mkBinary
      , mkDefault
      , mkMessage
      , mkStamped
      , mkLenses
      ]
  where Done _ msg = P.parse P.rosmsg (pack txt)
        mkDataName = mkName . drop 1 . last . groupBy (const isAlphaNum)
        msgRun n   = ($ (n, msg)) . uncurry

quoteMsgExp :: String -> ExpQ
quoteMsgExp txt = stringE (show msg)
  where Done _ msg = P.parse P.rosmsg (pack txt)

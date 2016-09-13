-- |
-- Module      :  Robotics.ROS.Msg.TH
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Template Haskell driven code generator from ROS message language
-- to Haskell native representation.
--
-- >>> [rosmsgFrom|/opt/ros/jade/share/std_msgs/msg/Byte.msg|]
-- "[Variable (Simple RByte,\"data\")]"
--
-- >>> [rosmsgFrom|/opt/ros/jade/share/geometry_msgs/msg/Polygon.msg|]
-- "[Variable (Array (Custom \"Point32\"),\"points\")]"
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Robotics.ROS.Msg.TH (
  -- * Native Haskell ROS message codegen 
    rosmsg
  , rosmsgFrom
  ) where

import           Data.Char (isAlphaNum, toLower)
import           Data.Default.Class (def)
import           Data.Maybe (catMaybes)
import           Data.Text.Lazy (pack)
import           Data.List (groupBy)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Lens.Family as L
import qualified Data.Text as T

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH

import qualified Robotics.ROS.Msg.Parser as Parser
import           Robotics.ROS.Msg.Render (renderT)
import           Robotics.ROS.Msg.Types
import           Robotics.ROS.Msg.MD5
import           Robotics.ROS.Msg

-- | Generate ROS message declarations from .msg file
rosmsgFrom :: QuasiQuoter
rosmsgFrom = quoteFile rosmsg

-- | QQ for data type and instances generation
-- from ROS message declaration
rosmsg :: QuasiQuoter
rosmsg = QuasiQuoter
  { quoteDec  = quoteMsgDec
  , quoteExp  = quoteMsgExp
  , quotePat  = undefined
  , quoteType = undefined
  }

-- | Take user type from text type name
customType :: Text -> TypeQ
customType = conT . mkName . T.unpack . qualify
  where qualify t = t <> "." <> t

-- | Field to Type converter
typeQ :: FieldType -> TypeQ
typeQ (Simple t)       = conT $ mkName $ mkFlatType t
typeQ (Custom t)       = customType t
typeQ (Array t)        = [t|ROSArray $(typeQ t)|]
typeQ (FixedArray l t) = [t|ROSFixedArray $arrSize $(typeQ t)|]
  where arrSize = litT $ numTyLit $ fromIntegral l

-- | Ensure that field and constant names are valid Haskell identifiers
-- and do not coincide with Haskell reserved words.
sanitizeField :: FieldDefinition -> FieldDefinition
sanitizeField (Constant (a, b) c) = Constant (a, sanitize b) c
sanitizeField (Variable (a, b))   = Variable (a, sanitize b)

-- | Sanitize identifier for valid Haskell
sanitize :: Text -> Text
sanitize x | isKeyword x = T.cons '_' x
           | otherwise   = T.toLower (T.take 1 x) <> T.drop 1 x
  where isKeyword = flip elem [ "as", "case", "of", "class"
                              , "data", "family", "instance"
                              , "default", "deriving", "do"
                              , "forall", "foreign", "hiding"
                              , "if", "then", "else", "import"
                              , "infix", "infixl", "infixr"
                              , "let", "in", "mdo", "module"
                              , "newtype", "proc", "qualified"
                              , "rec", "type", "where"]

-- | Generate the name of the Haskell type that corresponds to a flat
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

-- | Default value of field
defField :: FieldDefinition -> Maybe ExpQ
defField (Constant _ _)    = Nothing
defField (Variable (a, _)) = Just (defValue a)
  where defValue :: FieldType -> ExpQ
        defValue (Array _)        = [|ROSArray mempty|]
        defValue (FixedArray l t) = [|ROSFixedArray (replicate l $(defValue t))|]
        defValue (Simple RBool)   = [|False|]
        defValue (Simple RString) = [|""|]
        defValue _                = [|def|]

-- | Field definition to record var converter
fieldQ :: FieldDefinition -> Maybe VarStrictTypeQ
fieldQ (Constant _ _)         = Nothing 
fieldQ (Variable (typ, name)) = Just $ varStrictType recName recType
  where recName = mkName ('_' : T.unpack name)
        recType = strictType notStrict (typeQ typ)

-- | Generate the 'getSource' 'Message' class implementation
mkGetSource :: MsgDefinition -> DecQ
mkGetSource msg =
    funD' "getSource" [wildP] (renderString msg)
  where
    renderString = stringE . T.unpack . renderT

-- | Take list of external types and it's TypeQ
--
-- XXX: Original python @genmsg@ implementation ignore arrays
-- for hashing, it seems that types `A` and `A[]` has the same hash.
--
userTypes :: MsgDefinition -> [(ExpQ, TypeQ)]
userTypes = catMaybes . fmap go
  where
    textE = stringE . T.unpack
    go x = case x of
        Variable (Custom t, _) -> Just (textE t, customType t)
        Variable (Array (Custom t), _) -> Just (textE (t <> "[]"), customType t)
        Variable (FixedArray l (Custom t), _) ->
            Just (textE (t <> "[" <> T.pack (show l) <> "]"), customType t)
        _ -> Nothing

-- | Generate the 'getDigest' 'Message' class implementation
mkGetDigest :: MsgDefinition -> DecQ
mkGetDigest msg =
    funD' "getDigest" [] [|computeMD5 $digestMap . getSource|]
  where
    digestMap = listE (digestPair <$> userTypes msg)
    digestPair (name, typ) = [|($name, getDigest (undefined :: $typ))|]

-- | Generate the getType Message class implementation 
mkGetType :: DecQ
mkGetType = do
    l_mod <- loc_module <$> location
    let msgType = let [m, p] = fmap (drop 1) $ take 2 $
                               reverse $ groupBy (const (/= '.')) l_mod
                   in fmap toLower p ++ "/" ++ m
    funD' "getType" [wildP] [|msgType|]

-- | Lens signature
lensSig :: String -> TypeQ -> TypeQ -> DecQ
lensSig name a b = sigD (mkName name)
    [t|forall f. Functor f => ($b -> f $b) -> $a -> f $a|]

-- | Given a record field name,
-- produces a single function declaration:
-- lensName :: forall f. Functor f => (a -> f a') -> b -> f b' 
-- lensName f a = (\x -> a { field = x }) `fmap` f (field a)
-- FROM: Lens.Family.THCore
deriveLens :: Name -> FieldDefinition -> [DecQ]
deriveLens _ (Constant _ _) = []
deriveLens dataName (Variable (typ, name)) =
    [ lensSig (T.unpack name) (conT dataName) (typeQ typ)
    , funD' (T.unpack name) pats body]
  where a = mkName "a"
        f = mkName "f"
        fieldName = mkName ('_' : T.unpack name) 
        pats = [varP f, varP a]
        body = [| (\x -> $(record a fieldName [|x|]))
                  <$> $(appE (varE f) (appE (varE fieldName) (varE a)))
                |]
        record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

-- | Instance declaration with empty context
instanceD' :: Name -> TypeQ -> [DecQ] -> DecQ
instanceD' name insType insDecs =
    instanceD (cxt []) (appT insType (conT name)) insDecs

-- | Simple data type declaration with one constructor
dataD' :: Name -> ConQ -> [Name] -> DecQ
dataD' name rec derive =
#if MIN_VERSION_template_haskell(2,10,0)
    dataD (cxt []) name [] [rec] derive
#else
    dataD (cxt []) name [] Nothing [rec] $ cxt (conT <$> derive)
#endif

-- | Simple function declaration
funD' :: String -> [PatQ] -> ExpQ -> DecQ
funD' name p f = funD (mkName name) [clause p (normalB f) []]

-- | Lenses declarations
mkLenses :: Name -> MsgDefinition -> [DecQ]
mkLenses name msg =
    concat (deriveLens name . sanitizeField <$> msg)

-- | Data type declaration
mkData :: Name -> MsgDefinition -> [DecQ]
mkData name msg = pure $
    dataD' name (recC name fieldTypes) derivingD
  where
    fieldTypes = catMaybes (fieldQ . sanitizeField <$> msg)
    derivingD  = [ mkName "P.Show", mkName "P.Eq", mkName "P.Ord"
                 , mkName "Generic", mkName "Data", mkName "Typeable"
                 ]

-- | Binary instance declaration
mkBinary :: Name -> a -> [DecQ]
mkBinary name _ = pure $
    instanceD' name binaryT []
  where
    binaryT    = conT (mkName "Binary")

-- | Default instance declaration
mkDefault :: Name -> MsgDefinition -> [DecQ]
mkDefault name msg = pure $
    instanceD' name defaultT [defFun]
  where
    defaultT = conT (mkName "Default")
    defaults = catMaybes (defField <$> msg)
    defFun   = funD' "def" [] $ appsE (conE name : defaults)

-- | Message instance declaration
mkMessage :: Name -> MsgDefinition -> [DecQ]
mkMessage name msg = pure $
    instanceD' name messageT [ mkGetType
                             , mkGetSource msg
                             , mkGetDigest msg ]
  where
    messageT = conT (mkName "Message")

-- | Stamped instance declaration
mkStamped :: Name -> MsgDefinition -> [DecQ]
mkStamped name msg | hasHeader msg = pure go
                   | otherwise = []
  where
    hasHeader (Variable (Custom "Header", "header") : _) = True
    hasHeader _ = False

    seqL    = dyn "Header.seq"
    stampL  = dyn "Header.stamp"
    frameL  = dyn "Header.frame_id"
    headerL = dyn "header"

    mkSetSequence = funD' "setSequence" [] [|L.set  ($headerL . $seqL)|]
    mkGetSequence = funD' "getSequence" [] [|L.view ($headerL . $seqL)|]
    mkGetStamp    = funD' "getStamp"    [] [|L.view ($headerL . $stampL)|]
    mkGetFrame    = funD' "getFrame"    [] [|L.view ($headerL . $frameL)|]

    stampedT = conT (mkName "Stamped")

    go = instanceD' name stampedT [ mkGetSequence
                                  , mkSetSequence
                                  , mkGetStamp
                                  , mkGetFrame ]

-- | TemplateHaskell codegen from the ROS message language
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
  where Parser.Done _ msg = Parser.parse Parser.rosmsg (pack txt)
        mkDataName = mkName . drop 1 . last . groupBy (const isAlphaNum)
        msgRun n   = ($ (n, msg)) . uncurry

-- | Simple parse ROS message and show
quoteMsgExp :: String -> ExpQ
quoteMsgExp txt = stringE (show msg)
  where Parser.Done _ msg = Parser.parse Parser.rosmsg (pack txt)

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

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Attoparsec.Text.Lazy (Result(..))
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Char (isAlphaNum, toLower)
import           Data.Text.Lazy (pack, unpack)
import           Data.Digest.Pure.MD5 (md5)
import           Data.Maybe (catMaybes)
import           Text.Printf (printf)
import           Data.List (groupBy)
import           Data.Default (def)
import           Data.Monoid ((<>))
import qualified Lens.Family2 as L
import qualified Data.Text as T

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH

import qualified Robotics.ROS.Msg.Parser as Parser
import           Robotics.ROS.Msg.Render (render')
import           Robotics.ROS.Msg.Types
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
customType :: T.Text -> TypeQ
customType = conT . mkName . T.unpack . qualify . pkgInTypeHook
  where -- Some messages (e.g. geometry_msgs/Inertia in `com` field)
        -- contains package name in field type declarations
        -- it's too strange but exits now, this fix drop
        -- package name from type declaration
        pkgInTypeHook = last . T.split (== '/')
        qualify t     = t <> "." <> t

-- | Take list of external types used in message
externalTypes :: MsgDefinition -> [TypeQ]
externalTypes msg = customType <$> catMaybes (go <$> msg)
  where
    go (Variable (Custom t, _))                = Just t
    go (Variable (Array (Custom t), _))        = Just t
    go (Variable (FixedArray _ (Custom t), _)) = Just t
    go _ = Nothing

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
defValue :: FieldDefinition -> Maybe ExpQ
defValue (Constant _ _) = Nothing
defValue (Variable (Simple t, _)) = Just $
    case t of
        RBool     -> [|False|]
        RString   -> [|""|]
        _         -> [|def|]
defValue (Variable _) = Just [|def|]

-- | Field definition to record var converter
fieldQ :: FieldDefinition -> Maybe VarStrictTypeQ
fieldQ (Constant _ _)         = Nothing 
fieldQ (Variable (typ, name)) = Just $ varStrictType recName recType
  where recName = mkName ('_' : T.unpack name)
        recType = strictType notStrict (typeQ typ)

-- | Generate the getDigest Message class implementation 
mkGetDigest :: MsgDefinition -> DecQ
mkGetDigest msg =
    funD' "getDigest" [wildP] [| md5 (LBS.pack $(appsE source)) |]
  where
    source      = ([|printf $(stringE (render msg))|]
                : (depDigest <$> externalTypes msg))
    depDigest t = [|show (getDigest (undefined :: $(t)))|]
    render      = unpack . toLazyText . render' (const "%s")

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
#if __GLASGOW_HASKELL__ < 800
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
    dataD' name recs derivingD
  where
    fields     = sanitizeField <$> msg
    recs       = recC name (catMaybes (fieldQ <$> fields))
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
    defaults = catMaybes (defValue . sanitizeField <$> msg)
    defFun   = funD' "def" [] $ appsE (conE name : defaults)

-- | Message instance declaration
mkMessage :: Name -> MsgDefinition -> [DecQ]
mkMessage name msg = pure $
    instanceD' name messageT [mkGetDigest msg, mkGetType]
  where
    messageT = conT (mkName "Message")

-- | Stamped instance declaration
mkStamped :: Name -> MsgDefinition -> [DecQ]
mkStamped name msg | hasHeader msg = pure go
                   | otherwise = []
  where
    hasHeader [Variable (Custom "Header", _), _] = True
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
  where Done _ msg = Parser.parse Parser.rosmsg (pack txt)
        mkDataName = mkName . drop 1 . last . groupBy (const isAlphaNum)
        msgRun n   = ($ (n, msg)) . uncurry

-- | Simple parse ROS message and show
quoteMsgExp :: String -> ExpQ
quoteMsgExp txt = stringE (show msg)
  where Done _ msg = Parser.parse Parser.rosmsg (pack txt)

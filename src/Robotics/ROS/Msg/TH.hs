module Robotics.ROS.Msg.TH
  ( rosmsg
--  , rosmsgFrom
  ) where

import Data.Attoparsec.Text.Lazy (Result(..))
import qualified Data.Text as T
import Data.Text.Lazy (pack)
import Data.Char (isAlphaNum)
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import Control.Arrow ((&&&))
import Language.Haskell.TH.Quote
import Language.Haskell.TH

import qualified Robotics.ROS.Msg.Parser as P
import Robotics.ROS.Msg.Types

--rosmsgFrom :: QuasiQuoter
--rosmsgFrom = quoteFile rosmsg

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
typeQ (Array t)        = appT listT (typeQ t)
typeQ (FixedArray l t) = typeQ (Array t) -- TODO: type size annotation

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

getDigest :: String -> DecQ
getDigest src = funD (mkName "getDigest") [
    clause [wildP] (normalB (appE md5 (stringE src))) []] 
  where md5 = varE (mkName "md5")

getType :: String -> DecQ
getType t = funD (mkName "getType") [
    clause [wildP] (normalB (stringE t)) []] 


quoteMsgDec :: String -> Q [Dec]
quoteMsgDec txt = do
    (l_pkg, l_mod) <- (loc_package &&& loc_module) <$> location

    let dataName        = mkName $ drop 1 $ last $ groupBy (const isAlphaNum) l_mod
        dataRecords     = recC dataName (catMaybes (fieldQ <$> msg))
        binaryInstanceT = appT binaryC  (conT dataName)
        messageInstanceT= appT messageC (conT dataName)

    msgData <- dataD (cxt []) dataName [] [dataRecords] derivingD
    binInst <- instanceD (cxt []) binaryInstanceT [] 
    msgInst <- instanceD (cxt []) messageInstanceT [getDigest txt, getType l_mod]

    return [msgData, binInst, msgInst]
  where Done _ msg  = P.parse P.rosmsg (pack txt)
        derivingD = [mkName "Show", mkName "Eq", mkName "Generic"]
        binaryC   = conT (mkName "Binary")
        messageC  = conT (mkName "Message")

quoteMsgExp :: String -> ExpQ
quoteMsgExp txt = stringE (show msg)
  where Done _ msg = P.parse P.rosmsg (pack txt)

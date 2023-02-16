{-# OPTIONS_GHC -Wwarn=unused-imports #-}
{-# OPTIONS_GHC -Wwarn=unused-top-binds #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Type.Reflection (Typeable)

import qualified PlutusLedgerApi.V2 as V2
import Ply.Core.Typename (Typename (Typename), typeName)

data CTLEquivalent = forall a.
  Typeable a =>
  CTLEquivalent
  { cModule :: Text
  , cName :: Text
  , cType :: Proxy a
  }

ctlEquivalents :: [CTLEquivalent]
ctlEquivalents =
  [ CTLEquivalent "" "Boolean" (Proxy @Bool)
  , CTLEquivalent "" "BigInt" (Proxy @Integer)
  , CTLEquivalent "" "Unit" (Proxy @())
  , CTLEquivalent "" "RawBytes" (Proxy @V2.BuiltinByteString)
  , CTLEquivalent "" "RawBytes" (Proxy @ByteString)
  , CTLEquivalent "" "String" (Proxy @Text)
  , CTLEquivalent "" "PlutusData" (Proxy @V2.Data)
  , CTLEquivalent "" "Value" (Proxy @V2.Value)
  , CTLEquivalent "" "Address" (Proxy @V2.Address)
  , CTLEquivalent "" "CurrencySymbol" (Proxy @V2.CurrencySymbol)
  , CTLEquivalent "" "TokenName" (Proxy @V2.TokenName)
  , CTLEquivalent "" "PubKeyHash" (Proxy @V2.PubKeyHash)
  , CTLEquivalent "" "ScriptHash" (Proxy @V2.ScriptHash)
  , CTLEquivalent "" "POSIXTime" (Proxy @V2.POSIXTime)
  , CTLEquivalent "" "DCert" (Proxy @V2.DCert)
  , CTLEquivalent "" "TxId" (Proxy @V2.TxId)
  , CTLEquivalent "" "TransactionInput" (Proxy @V2.TxOutRef)
  , CTLEquivalent "" "TransactionOutput" (Proxy @V2.TxOut)
  , -- , CTLEquivalent "" "(Fail (Text \"TxInInfo is not provided in CTL\"))" (Proxy @V2.TxInInfo)
    -- , CTLEquivalent "" "(Fail (Text \"TxInfo is not provided in CTL\"))" (Proxy @V2.TxInfo)
    -- , CTLEquivalent "" "(Fail (Text \"ScriptPurpose is not provided in CTL\"))" (Proxy @V2.ScriptPurpose)
    -- , CTLEquivalent "" "(Fail (Text \"ScriptContext is not provided in CTL\"))" (Proxy @V2.ScriptContext)
    CTLEquivalent "" "Datum" (Proxy @V2.Datum)
  , CTLEquivalent "" "Datum" (Proxy @V2.Redeemer)
  , CTLEquivalent "" "DatumHash" (Proxy @V2.DatumHash)
  , CTLEquivalent "" "RedeemerHash" (Proxy @V2.RedeemerHash)
  ]

genCTLEquiv :: CTLEquivalent -> Text
genCTLEquiv (CTLEquivalent _ n (_ :: Proxy a)) =
  let (Typename tName) = typeName @a
   in "instance PlyTypeName " <> n <> " where plyTypeName = \"" <> tName <> "\""

main :: IO ()
main = do
  mapM_ putStrLn $ T.unpack . genCTLEquiv <$> ctlEquivalents

-- $> main

{-
instance PlyTypeName Boolean where plyTypeName = "GHC.Types:Bool"
instance PlyTypeName BigInt where plyTypeName = "GHC.Num.Integer:Integer"
instance PlyTypeName Unit where plyTypeName = "GHC.Tuple:()"
instance PlyTypeName RawBytes where plyTypeName = "PlutusTx.Builtins.Internal:BuiltinByteString"
instance PlyTypeName RawBytes where plyTypeName = "Data.ByteString.Internal:ByteString"
instance PlyTypeName String where plyTypeName = "Data.Text.Internal:Text"
instance PlyTypeName PlutusData where plyTypeName = "PlutusCore.Data:Data"
instance PlyTypeName Value where plyTypeName = "PlutusLedgerApi.V1.Value:Value"
instance PlyTypeName Address where plyTypeName = "PlutusLedgerApi.V1.Address:Address"
instance PlyTypeName CurrencySymbol where plyTypeName = "PlutusLedgerApi.V1.Value:CurrencySymbol"
instance PlyTypeName TokenName where plyTypeName = "PlutusLedgerApi.V1.Value:TokenName"
instance PlyTypeName PubKeyHash where plyTypeName = "PlutusLedgerApi.V1.Crypto:PubKeyHash"
instance PlyTypeName ScriptHash where plyTypeName = "PlutusLedgerApi.V1.Scripts:ScriptHash"
instance PlyTypeName POSIXTime where plyTypeName = "PlutusLedgerApi.V1.Time:POSIXTime"
instance PlyTypeName DCert where plyTypeName = "PlutusLedgerApi.V1.DCert:DCert"
instance PlyTypeName TxId where plyTypeName = "PlutusLedgerApi.V1.Tx:TxId"
instance PlyTypeName TransactionInput where plyTypeName = "PlutusLedgerApi.V1.Tx:TxOutRef"
instance PlyTypeName TransactionOutput where plyTypeName = "PlutusLedgerApi.V2.Tx:TxOut"
instance PlyTypeName Datum where plyTypeName = "PlutusLedgerApi.V1.Scripts:Datum"
instance PlyTypeName Datum where plyTypeName = "PlutusLedgerApi.V1.Scripts:Redeemer"
instance PlyTypeName DatumHash where plyTypeName = "PlutusLedgerApi.V1.Scripts:DatumHash"
instance PlyTypeName RedeemerHash where plyTypeName = "PlutusLedgerApi.V1.Scripts:RedeemerHash"
-}

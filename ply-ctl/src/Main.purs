module Main where

import Contract.Prim.ByteArray
import Contract.Prelude
import Contract.Value
import Aeson as Aeson
import Effect
import Ply.Typename
import Ply.Reify
import Ply.Types
import Type.Proxy
import Contract.Transaction
import Ply.TypeList
import Ply.Apply as PA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Node.FS.Sync
import Node.Encoding

main :: Effect Unit
main = do
  sample <- readTextFile UTF8 "/home/sho/Documents/ply/example/compiled/nftMp.plutus"
  t :: TypedScriptEnvelope <-
    Aeson.parseJsonStringToAeson sample
      # fromRightEff
      >>= Aeson.decodeAeson
      >>> fromRightEff
  txid <- hexToByteArray "2be7c999fda3c9d4c3540bc9f4f28b78f8aacf9662b4489d8000bcdc18131268" # fromJustEff "txid"
  tokenName <- byteArrayFromAscii "A" >>= mkTokenName # fromJustEff "tkname"
  let
    ts :: TypedScript MintingPolicyRole (Cons (AsData TransactionInput) (Cons (AsData TokenName) Nil))
    ts = reifyTypedScript t

    arg1 =
      wrap
        { index: UInt.fromInt 1
        , transactionId: wrap txid
        }

    arg2 = adaToken
  applied <- ts PA.# arg1 PA.# arg2 # toPlutusScript # fromRightEff
  log $ show applied

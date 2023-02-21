module ApplicationTest where

import Contract.Prelude
import Contract.Prim.ByteArray

import Aeson as Aeson
import Node.Process (argv)
import Node.FS.Sync
import Node.Encoding
import Data.Int
import Data.Array
import Ply.Typename
import Ply.Reify
import Ply.Types
import Ply.TypeList
import Ply.Apply as PA
import Data.BigInt (BigInt)
import Data.BigInt as BigInt

foreign import stdoutWrite :: String -> Effect Unit

main :: Effect Unit
main = do
  args <- argv

  arg <- args !! 1 >>= fromString # fromJustEff "need argument"

  sample <-
    readTextFile UTF8 "./application-test/compiled/SampleA.plutus"

  t :: TypedScriptEnvelope <-
    Aeson.parseJsonStringToAeson sample
      # fromRightEff
      >>= Aeson.decodeAeson
      >>> fromRightEff

  let
    ts :: TypedScript MintingPolicyRole (Cons (AsData BigInt) Nil)
    ts = reifyTypedScript t

  appliedScript <-
    ts PA.# (BigInt.fromInt arg)
      # toPlutusScript
      # fromRightEff
      <#> unwrap
      >>> fst
      >>> byteArrayToHex

  stdoutWrite appliedScript

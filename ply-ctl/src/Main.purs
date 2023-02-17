module Main where

import Prelude

import Contract.Prelude
import Contract.Value
import Aeson as Aeson
import Effect
import Ply.Typename
import Ply.Reify
import Ply.Types
import Type.Proxy
import Contract.Transaction (TransactionHash,TransactionInput,TransactionOutput)
import Ply.TypeList

import Node.FS.Sync
import Node.Encoding (Encoding (UTF8))


main :: Effect Unit
main = do
  sample <- readTextFile UTF8 "/home/sho/Documents/ply/example/compiled/nftMp.plutus"

  log sample

  t :: TypedScriptEnvelope <- Aeson.parseJsonStringToAeson sample
    # fromRightEff
    >>= Aeson.decodeAeson
    >>> fromRightEff

  log $ show t

  log $ show $ reifyParams (Proxy :: Proxy (Cons Boolean (Cons TransactionHash Nil)))

  log $ show $ reifyRole (Proxy :: Proxy ValidatorRole)

  ts :: TypedScript MintingPolicyRole (Cons TransactionInput (Cons TokenName Nil)) <- reifyTypedScript t # fromRightEff

  log $ plyTypeName (Proxy :: Proxy Boolean)
  log $ plyTypeName (Proxy :: Proxy TransactionHash)
  log $ plyTypeName (Proxy :: Proxy TransactionInput)
  log $ plyTypeName (Proxy :: Proxy TransactionOutput)
  log "hello world"

module Main (main) where

import System.FilePath ((</>))

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import PlutusLedgerApi.V2
import Ply
import UntypedPlutusCore

data MintingPolicy = MintingPolicy (Program DeBruijn DefaultUni DefaultFun ())

instance Show MintingPolicy where
  show =
    Text.unpack
      . Text.decodeUtf8
      . Base16.encode
      . SBS.fromShort
      . serialiseUPLC
      . (\(MintingPolicy x) -> x)

toMintingPolicy :: TypedScript 'MintingPolicyRole '[] -> MintingPolicy
toMintingPolicy (TypedScript _ s) = MintingPolicy s

usePolicy :: MintingPolicy -> IO ()
usePolicy _ = putStrLn "Pretending to build and submit a transaction"

main :: IO ()
main = do
  nftMp <- readTypedScript $ "compiled" </> "nftMp.plutus"

  -- Print the Plutus ledger version used by the minting policy.
  putStr "NFT Minting Policy version: "
  print $ getPlutusVersion nftMp

  let policy =
        toMintingPolicy $
          nftMp
            #! TxOutRef {txOutRefId = "2be7c999fda3c9d4c3540bc9f4f28b78f8aacf9662b4489d8000bcdc18131268", txOutRefIdx = 0}
            #! ("A" :: TokenName)

  print policy

  usePolicy policy

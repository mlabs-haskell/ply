module Main (main) where

import qualified Data.ByteString.Short as SBS
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import qualified Data.Text as Txt
import System.FilePath ((</>))

import qualified Cardano.Binary as CBOR
import Plutarch.Internal.Term (Config (Tracing), LogLevel (LogInfo), TracingMode (DoTracing), compile)
import Plutarch.LedgerApi.V3 (PScriptContext, PTokenName, PTxOutRef, scriptHash)
import Plutarch.Prelude (PAsData, PData, POpaque, (:-->))
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V3 (ScriptHash (..))
import PlutusTx.Blueprint
import Ply (reifyVersion)
import Ply.Plutarch

-- This is apparently the V2 script hash.
-- https://github.com/Plutonomicon/plutarch-plutus/blob/75c06ef1e77742916574414975f76ddac59cce4a/plutarch-ledger-api/src/Plutarch/LedgerApi/V3.hs#L152C1-L152C11
import qualified PlutusTx.Builtins as PlutusTx

import Example.NftM (nftMp)

main :: IO ()
main =
  writeBlueprint
    ("compiled" </> "nftMp.plutus")
    MkContractBlueprint
      { contractId = Nothing
      , contractPreamble =
          MkPreamble
            { preambleTitle = "Example Contract"
            , preambleDescription = Nothing
            , preambleVersion = "1.0.0"
            , preamblePlutusVersion = reifyVersion $ Proxy @(VersionOf (PTxOutRef :--> PAsData PTokenName :--> PData :--> PScriptContext :--> POpaque))
            , preambleLicense = Nothing
            }
      , contractValidators = Set.singleton scriptBP
      , contractDefinitions = derivePDefinitions @(ParamsOf (PTxOutRef :--> PAsData PTokenName :--> PData :--> PScriptContext :--> POpaque))
      }
  where
    scriptBP =
      -- Though this minting policy doesn't actually need a redeemer, we still need to declare one. So we choose 'PTxOutRef' as the redeemer type.
      MkValidatorBlueprint
        { validatorTitle = "NftMP"
        , validatorDescription = Nothing
        , validatorParameters =
            map
              ( \sch ->
                  MkParameterBlueprint
                    { parameterTitle = Nothing
                    , parameterDescription = Nothing
                    , parameterPurpose = Set.fromList [Spend, Mint]
                    , parameterSchema = sch
                    }
              )
              $ mkParamSchemas @(ReferencedTypesOf' PTxOutRef (PTxOutRef :--> PAsData PTokenName :--> PData :--> PScriptContext :--> POpaque)) @(ParamsOf (PTxOutRef :--> PAsData PTokenName :--> PData :--> PScriptContext :--> POpaque))
        , validatorRedeemer =
            MkArgumentBlueprint
              { argumentTitle = Nothing
              , argumentDescription = Nothing
              , argumentPurpose = Set.fromList [Spend, Mint]
              , argumentSchema = definitionRef @(PlyArgOf PTxOutRef)
              }
        , validatorDatum = Nothing
        , validatorCompiled =
            Just
              MkCompiledValidator
                { compiledValidatorHash = PlutusTx.fromBuiltin hash
                , compiledValidatorCode = CBOR.serialize' . SBS.fromShort $ serialiseScript script
                }
        }
    script = either (error . Txt.unpack) id $ compile (Tracing LogInfo DoTracing) nftMp
    -- NOTE: Plutarch's scriptHash function is exported from V3 but hashes with V2 prefix. This is a bug.
    ScriptHash hash = scriptHash script

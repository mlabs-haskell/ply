module Main (main) where

import qualified Data.ByteString.Short as SBS
import qualified Data.Set as Set
import qualified Data.Text as Txt
import System.FilePath ((</>))

import qualified Cardano.Binary as CBOR
import Plutarch.Internal.Term (Config (Tracing), LogLevel (LogInfo), TracingMode (DoTracing), compile)
import Plutarch.LedgerApi.V2 (PScriptContext, PTokenName, PTxOutRef)
import Plutarch.Prelude (PData, POpaque, PUnit, (:-->))
import Plutarch.Script (serialiseScript)
import PlutusTx.Blueprint
import Ply.Plutarch

-- This is apparently the V2 script hash.
-- https://github.com/Plutonomicon/plutarch-plutus/blob/75c06ef1e77742916574414975f76ddac59cce4a/plutarch-ledger-api/src/Plutarch/LedgerApi/V3.hs#L152C1-L152C11
import Plutarch.LedgerApi.V3 (scriptHash)
import PlutusLedgerApi.V3 (ScriptHash (..))
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
            , preamblePlutusVersion = PlutusV2
            , preambleLicense = Nothing
            }
      , contractValidators = Set.singleton scriptBP
      , contractDefinitions = derivePDefinitions @(PUnit : ParamsOf (PTxOutRef :--> PTokenName :--> PData :--> PScriptContext :--> POpaque))
      }
  where
    scriptBP =
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
              $ mkParamSchemas @(ReferencedTypesOf' PUnit (PTxOutRef :--> PTokenName :--> PData :--> PScriptContext :--> POpaque)) @(ParamsOf (PTxOutRef :--> PTokenName :--> PData :--> PScriptContext :--> POpaque))
        , validatorRedeemer =
            MkArgumentBlueprint
              { argumentTitle = Nothing
              , argumentDescription = Nothing
              , argumentPurpose = Set.fromList [Spend, Mint]
              , argumentSchema = definitionRef @(PlyArgOf PUnit)
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
    ScriptHash hash = scriptHash script

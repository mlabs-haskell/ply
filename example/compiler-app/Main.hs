{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import qualified Data.ByteString.Short as SBS
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import qualified Data.Text as Txt
import System.FilePath ((</>))

import qualified Cardano.Binary as CBOR
import Plutarch.Internal.Term (ClosedTerm, Config (Tracing), LogLevel (LogInfo), TracingMode (DoTracing), compile)
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V3 (ScriptHash (ScriptHash))
import PlutusTx.Blueprint
import Ply (ReifyVersion, reifyVersion)
import Ply.Plutarch

-- This is apparently the V2 script hash.
-- https://github.com/Plutonomicon/plutarch-plutus/blob/75c06ef1e77742916574414975f76ddac59cce4a/plutarch-ledger-api/src/Plutarch/LedgerApi/V3.hs#L152C1-L152C11
import qualified PlutusTx.Builtins as PlutusTx

import Example.NftM (nftMp)
import Plutarch.Prelude (PData)

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
            , preamblePlutusVersion = versionOf nftMp
            , preambleLicense = Nothing
            }
      , contractValidators = Set.singleton scriptBP
      , contractDefinitions = scriptDefinitions @PData nftMp
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
              $ scriptParamSchemas @PData nftMp
        , validatorRedeemer =
            MkArgumentBlueprint
              { argumentTitle = Nothing
              , argumentDescription = Nothing
              , argumentPurpose = Set.fromList [Spend, Mint]
              , -- In this case, the redeemer type does not matter - so we're just going with "any".
                -- But you may want to change this based on your script.
                argumentSchema = definitionRef @(PlyArgOf PData)
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

versionOf :: forall ptype. ReifyVersion (VersionOf ptype) => ClosedTerm ptype -> PlutusVersion
versionOf _ = reifyVersion $ Proxy @(VersionOf ptype)

-- Note: We have to manually prepend datum/redeemer to the types because it does not exist on the Plutarch type.
scriptDefinitions :: forall redeemer ptype. HasDefinitions (redeemer : ParamsOf ptype) => ClosedTerm ptype -> Definitions (ReferencedTypesOf (redeemer : ParamsOf ptype))
scriptDefinitions _ = derivePDefinitions @(redeemer : ParamsOf ptype)

-- Note: When using 'mkParamSchemas', the second type argument should only contain the params (i.e from 'ParamsOf'), not the datum/redeemer.
scriptParamSchemas :: forall redeemer ptype. HasDefinitions (redeemer : ParamsOf ptype) => ClosedTerm ptype -> [Schema (ReferencedTypesOf (redeemer : ParamsOf ptype))]
scriptParamSchemas _ = mkParamSchemas @(ReferencedTypesOf (redeemer : ParamsOf ptype)) @(ParamsOf ptype)

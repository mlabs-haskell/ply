# Ply

Ply allows you to read Plutus blueprints produced by **Aiken** or **Plutarch** code into well-typed scripts in Haskell.

Seamlessly import Aiken produced blueprints into Haskell code with the confidence of type checking!

```hs
import Data.ByteString (ByteString)
import Data.Kind (Type)

import Cardano.Api qualified as C
import PlutusLedgerApi.Common (BuiltinByteString, serialiseUPLC)
import Ply
import qualified PlutusTx.Builtins as PlutusTx

type MintingPolicy lang redeemer = TypedScript lang '[AsRedeemer redeemer]
type Validator lang datum redeemer = TypedScript lang '[AsDatum datum, AsRedeemer redeemer]

data MyScripts = MyScripts
  { myMintingPolicy :: MintingPolicy PlutusV3 MyMintRedeemer
  , myValidator :: Validator PlutusV3 MySpendDatum MySpendRedeemer
  }

readAikenScripts :: IO MyScripts
readAikenScripts = do
  -- Note: plutus.json is the blueprint produced by `aiken build`
  aikenBp <- readBlueprint "plutus.json"
  -- Use the script name in the blueprint as per your Aiken code.
  -- It will read the parameterized minting policy.
  -- In this case, let's say my Aiken policy takes an extra bytestring parameter.
  -- e.g: `validator mint(my_parameter: ByteArray)`
  myMintingPolicy' <- getTypedScript aikenBp "my.mint.mint"
  -- The parameterized validator takes the previous minting policy id as argument.
  -- e.g: `validator spend(my_policy_id: PolicyId)`
  myValidator' <- getTypedScript aikenBp "my.spend.spend"
  -- Now, we apply the parameters and pack the ready-to-attach scripts into 'MyScripts'.
  let myParameter :: BuiltinByteString = ...
  let myMintingPolicy = myMintingPolicy' # myParameter
  let myValidator = myValidator' #$ PlutusTx.toBuiltin $ mintingPolicyIdBytes myMintingPolicy`
  pure MyScripts { myMintingPolicy, myValidator }

-- Type safe helper to get the PolicyId of a minting policy.
mintingPolicyIdBytes ::
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  MintingPolicy version anyRedeemer -> ByteString
mintingPolicyIdBytes = C.serialiseToRawBytes . C.scriptPolicyId . C.PlutusScript C.plutusScriptVersion . toMintingPolicy

-- Type safe helpers to guard validator and minting policies!
toValidator ::
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  Validator version anyDatum anyRedeemer -> C.PlutusScript (TransPlutusVersion version)
toValidator (TypedScript script) = C.PlutusScriptSerialised $ serialiseUPLC script

toMintingPolicy ::
  (C.IsPlutusScriptLanguage (TransPlutusVersion version)) =>
  MintingPolicy version anyRedeemer -> C.PlutusScript (TransPlutusVersion version)
toMintingPolicy (TypedScript' script) = C.PlutusScriptSerialised $ serialiseUPLC script

type TransPlutusVersion :: Ply.PlutusVersion -> Type
type family TransPlutusVersion x = r | r -> x where
  TransPlutusVersion Ply.PlutusV1 = C.PlutusScriptV1
  TransPlutusVersion Ply.PlutusV2 = C.PlutusScriptV2
  TransPlutusVersion Ply.PlutusV3 = C.PlutusScriptV3
```

Also see: [Example of using custom types with Ply](./example/common/Example/Type.hs)

Ply also provides functions to serialize Plutarch validators/minting policies (with optionally extra parameters) into Plutus blueprints according to **[CIP-57](https://cips.cardano.org/cip/CIP-57s)**.

This facilitates the onchain/offchain split that is often utilized, without forcing the user to manage the intricacies of the types and their UPLC representation when it comes to parameterized scripts. i.e scripts that take extra parameters before being submitted to the chain.

# Goals

- Efficiency: Applying constants with `Ply` should be equally efficient as using `pconstant`/`pconstantData` in the Plutarch function body directly.

  _However_, some protocols require **deterministic parameterization** - optimizations are not acceptable. In this case, Ply also provides the same ergonomics of application, with a different application function - which does not perform _any optimizations_.
- Ergonomics: Users shouldn't need to deal with UPLC intricacies. Users shouldn't have to care about micro-managing Plutus script versions (V1/V2/V3).
- Invariant validation: Before given Haskell constant is applied to the script, any invariant expressed within its CIP-57 are validated and any relevant normalization is performed.

  **NOTE**: This is a planned feature and has not been implemented yet.
- First class Plutarch support: Preference given to and ease of use with Plutarch written scripts.

  Thanks to the tight integration with Plutarch, Ply can introspect many things about a Plutarch script, including: Plutus version, extra parameters etc.
- Minimal dependencies: Not dependent on the entire `plutus-apps` stack, not dependent on problematic/conflicting dependencies that may prevent compilation in certain repositories.

# Adding as dependency

Ply uses [CHaP](https://github.com/input-output-hk/cardano-haskell-packages). Adding it as a dependency should generally be as simple as pointing to CHaP in your `cabal.project` (you can also just copy the `cabal.project` in this repo), and adding `ply` as a source-repository package. Set the `subdirs` to the components you specifically require (`ply-core` and/or `ply-plutarch`).

# Usage

Ply has 2 components: `ply-core`, and `ply-plutarch`.

`ply-core` is the package you'll be using in your offchain project to read Plutus blueprints produced by Aiken/Plutarch/Plinth etc. This way, your offchain will be able to keep track of the script types!

`ply-plutarch` is the package you'll be using in your onchain Plutarch project. Usually, you'll want to create an executable target in this project to compile your plutus scripts into the file system. These files can then be read back in your offchain project using `ply-core`.

## Type definition

See: [MyParameter](./example/common/Example/Type.hs)

## Onchain/Plutarch project

See: [example-compiler](./example/compiler-app/Main.hs)

## Offchain project

See: [example-reader](./example/reader-app/Main.hs)

## Offchain Projects in CTL

[CTL](https://github.com/plutonomicon/cardano-transaction-lib) is a popular alternative to PlutusTx with it being
directly compiled into Javascript.

Currently, [ply-ctl](https://github.com/mlabs-haskell/ply-ctl) supports reading CIP-57 blueprints but CTL integration is only _planned_.

# Custom Types as Script Parameters

By default, Ply supports most `plutus-ledger-api` types and you can use any of them as your script parameters.

If you want to use your custom type as a script parameter, you will need a lawful `PlyArg` instance for the Haskell type and a `PlyArgOf` instance on the Plutarch synonym, set to the Haskell type.

These can be automatically derived. See: [example-common](./example/common/) for an example. Also see `PMyParameter` in [compiler-app](./example/compiler-app/Main.hs) for the Plutarch side instance.

# Examples

You can find a full example demonstrating the usage of Ply on a simple nft minting contract inside [example](./example/).

Also, [Agora](https://github.com/liqwid-labs/agora) uses Ply for all of its script exportation.

# Shortcomings/Pitfalls

- Does not support applying a function (pre-compiled) as an argument yet.
- Several, more sophisticated optimizations are currently missing.
- No support for PlutusTx.

# Ply

Ply allows you to serialize your Plutarch validators/minting policies (with optionally extra parameters) into the file system, and read them back - preserving the type information.

This facilitates the onchain/offchain split that is often utilized, without forcing the user to manage the intricacies of the types and their UPLC representation when it comes to parameterized scripts. i.e scripts that take extra parameters before being submitted to the chain.

# Goals

- Efficiency: Applying constants with `Ply` should be equally efficient as using `pconstant`/`pconstantData` in the Plutarch function body directly.

  _However_, some protocols require **deterministic parameterization** - optimizations are not acceptable. In this case, Ply also provides the same ergonomics of application, with a different application function - which does not perform _any optimizations_.
- Ergonomics: Users shouldn't need to deal with UPLC intricacies. Users shouldn't have to care about micro-managing Plutus script versions (V1 and V2).
- Invariant validation: Before given Haskell constant is applied to the script, all its invariants are validated and any relevant normalization is performed. e.g `PValue` is sorted and normalized (zero elements removed) before being passed into the script.

  You can bestow _custom invariants_ for your **custom types** using `PlyArg`.
- First class Plutarch support: Preference given to and ease of use with Plutarch written scripts.

  Thanks to the tight integration with Plutarch, Ply can figure out whether a Plutarch validator/minting policy is PlutusV1 or PlutusV2 on its own!
- Minimal dependencies: Not dependent on the entire `plutus-apps` stack, not dependent on problematic/conflicting dependencies that may prevent compilation in certain repositories.

# Adding as dependency

Ply uses [CHaP](https://github.com/input-output-hk/cardano-haskell-packages). Adding it as a dependency should generally be as simple as pointing to CHaP in your `cabal.project` (you can also just copy the `cabal.project` in this repo), and adding `ply` as a source-repository package. Set the `subdirs` to the components you specifically require (`ply-core` and/or `ply-plutarch`).

# Usage

Ply has 2 components: `ply-core`, and `ply-plutarch`.

`ply-core` is the package you'll be using in your offchain project, where you cannot depend on Plutarch (potentially due to dependency conflicts). This comes with support for reading the serialized scripts with their types.

`ply-plutarch` is the package you'll be using in your onchain Plutarch project. Usually, you'll want to create an executable target in this project to compile your plutus scripts into the file system. These files can then be read back in your offchain project using `ply-core`.

## Onchain/Plutarch project
In your Plutarch project, use `writeTypedScript` to write your Plutarch term to a file.

This is what the type of `writeTypedScript` looks like:

```hs
writeTypedScript ::
  TypedWriter pt =>
  -- | Plutarch compiler configuration which will be used to compile the script.
  Config ->
  -- | Description for the file, semantically irrelevant - just for human comprehension!
  Text ->
  -- | The path where this file should be written
  FilePath ->
  -- | The Plutarch term itself, of course!
  ClosedTerm pt ->
  IO ()
```

> Aside: The `TypedWriter` constraint on the Plutarch type effectively barricades unsupported types. Of course, only validators and minting policies with 0 or more extra parameters are supported. The specific parameter types themselves also need to satisfy some constraints. See: [Using custom types as script parameters](#custom-types-as-script-parameters)

This is how it'd look in practice:

```hs
import Data.Default (def)

import Plutarch
import Plutarch.Builtin (pasInt)
import Plutarch.Prelude
import Plutarch.Api.V1

import Ply.Plutarch (writeTypedScript)

parameterizedLock :: ClosedTerm (PInteger :--> PData :--> PInteger :--> PScriptContext :--> PUnit)
parameterizedLock = plam $ \i datm redm ctx -> pif (redm #== i) (pconstant ()) $ ptraceError "incorrect guess"

parameterizedLockV :: ClosedTerm (PInteger :--> PValidator)
parameterizedLockV = plam $ \i datm redm ctx -> popaque $ parameterizedLock # i # datm # (pasInt # redm) # ctx

main :: IO ()
main =
  writeTypedScript def "Parameterized lock validator" "path/to/script.plutus" parameterizedLockV
```

Now you're ready to read the script from `path/to/script.plutus` in your offchain project!

## Offchain project

This is just as simple, use `readTypedScript` to read the script, given a path. However, you also have to specify what _type_ of script you're expecting (validator or minting policy), and what its extra parameters should be. `readTypedScript` will validate that your expected types match the actual types when reading the file - if it fails validation, a `ScriptReaderException` will be raised.

Here's the type of `readTypedScript`:

```hs
readTypedScript ::
  TypedReader r params =>
  -- | File path where the typed script file is located.
  FilePath ->
  IO (TypedScript r params)
```

As you can see, the `r` and the `params` should be specified by the caller (or inferred from the surrounding context). `r` is of kind `ScriptRole`, which should be either `'ValidatorRole` or `'MintingPolicyRole`. In this example, this is `'ValidatorRole` of course.

The `params` type variable is of kind `[Type]` - simply a type level list of `Type`s. These will be the corresponding Haskell types for the extra parameters your Plutarch validator/minting policy will take before it can be treated as a validator/minting policy. In this example, it's just one extra parameter: `Integer`, so `params` would be `'[Integer]`

In practice, a read _usually_ looks like:

```hs
import Ply

parameterizedLockV <- readTypedScript "path/to/script.plutus"
```

Generally, you'll be using up the `parameterizedLockV` in some way which will help GHC infer the `r` and the `params` automatically. However, you can choose to be explicit and use `TypeApplications` regardless:

```hs
parameterizedLockV <- readTypedScript @'ValidatorRole @'[Integer] "path/to/script.plutus"
```

Once you have the script itself - you can now work with it! You can apply the extra parameters to it at your leisure using `#` from `Ply`. When you're all done - you can convert it to a `Validator` or `MintingPolicy` using `Ply.toValidator` or `Ply.toMintingPolicy` respectively.

> Aside: As mentioned previously, if you require deterministic and predictable parameter application: please use `#!` (or any of its synonyms) instead of `#`. Those will not perform any optimizations and is guaranteed to use a raw UPLC `Apply` constructor wrapper.

Here's a full example of using a `TypedScript`, utilizing its Plutus version tracking, and applying the integer `42` as the extra parameter:

```hs
import Data.Text (Text)

import Plutus.Contract (Contract, EmptySchema)
import qualified Plutus.Contract as Contract
import Ledger.Constraints (ScriptLookups)
import qualified Ledger.Constraints as Constraints
import PlutusLedgerApi.V1.Scripts (Validator)

import Ply (ScriptRole(ValidatorRole), ScriptVersion(ScriptV1), readTypedScript, (#))
import qualified Ply

-- | Dispatch to either 'plutusV1OtherScript' or 'plutusV2OtherScript' depending on the script version.
otherTypedScript :: TypedScript ValidatorRole '[] -> ScriptLookups a
otherTypedScript ts = dispatcher vald
  where
    dispatcher = if ver == ScriptV1 then Constraints.plutusV1OtherScript else Constraints.plutusV2OtherScript
    ver = Ply.getPlutusVersion ts
    vald = Validator ts

someContract :: TypedScript ValidatorRole '[Integer] -> Contract () EmptySchema Text ()
someContract lockV = do
  let preparedValidator = lockV # param
      lookups = otherTypedScript preparedValidator
      tx = ... -- Some constraints
  void $ Contract.submitTxConstraintsWith @Void lookups tx
  where
    param :: Integer
    param = 42

runContract :: Contract w s e a -> IO a
runContract = ... -- imagine a contract runner impl!

main :: IO ()
main = do
  parameterizedLockV <- readTypedScript "path/to/script.plutus"
  runContract $ someContract parameterizedLockV
```

> Aside: Notice how I didn't use type applications, it got inferred from the surrounding context!

## Offchain Projects in CTL

[CTL](https://github.com/plutonomicon/cardano-transaction-lib) is a popular alternative to PlutusTx with it being
directly compiled into Javascript. For projects using CTL to write offchain contracts, Ply typed envelopes can be 
imported via [ply-ctl](https://github.com/mlabs-haskell/ply-ctl) with full type safety. For more information, 
check README on ply-ctl.

Note, ply-ctl will only work with scripts exported with Ply version `v0.5.0` or later.

## Building a TypedScript directly from Plutarch

`Ply.Plutarch.writeTypedScript` is essentially a wrapper around `Ply.Plutarch.mkEnvelope`, which is then a wrapper around `Ply.Plutarch.toTypedScript`.

Indeed, you can use `toTypedScript` on your Plutarch term to directly obtain a well typed `TypedScript`. This is useful when your offchain code is on the same project and can directly import `TypedScript`. The benefit of this over applying Plutarch arguments to Plutarch term, and then compiling in the end, is simple: the UPLC compilation (which is potentially a hefty task) is done only once. The arguments applied later are done by lightweight UPLC AST modifications.

# Custom Types as Script Parameters

By default, Ply supports most `plutus-ledger-api` types and you can use any of them as your script parameters.

If you want to use your custom type as a script parameter, you will need a lawful `PlyArg` instance for the Haskell type and a `PlyArgOf` instance on the Plutarch synonym, set to the Haskell type.

Eventually, there will be generic derivation support for this.

# How does Ply perform type validation across onchain <-> offchain? (Read this!)

Ply's onchain <-> offchain type validation isn't magic. All Ply can really do is ask the compiler for the qualified type name in string form, and put it inside the generated ".plutus" file. For the validation to pass, the qualified type names for each parameter has to match in both the offchain and the onchain project.

For example, if one of your parameter has type `MyParameterType`, and it was defined in the module: `MyPackage.Types` - then the generated parameter type in the ".plutus" envelope will contain `MyPackage.Types` and `MyParameterType` as the core identifiers. As a result, in your offchain project, `MyParameterType` should also have the same qualified type name.

This is usually the case in most projects, as custom types are usually defined in some common package shared by both offchain and onchain - so they always have the same qualified type name.

However, there is a huge exception: the plutus ledger api. Many outdated offchain projects will still be using the older ledger api namespace (`Plutus.Vx.Ledger`), and the onchain Plutarch project is going to be using the newer ledger api namespace (`PlutusLedgerApi`).

So `typeOf @CurrencySymbol`, for example, generates `PlutusLedgerApi.V1.Value` as its module name in the onchain project - but `Plutus.V1.Ledger.Value` on the outdated offchain project...

For this reason, Ply handles this case specially - you can read more on it [in the code](./ply-core/src/Ply/Core/Typename.hs). But the idea is that you _won't_ need to care about this one exception.

# Examples

You can find a full example demonstrating the usage of Ply on a simple nft minting contract inside [example](./example/).

Also, [Agora](https://github.com/liqwid-labs/agora) uses Ply for all of its script exportation.

# Shortcomings/Pitfalls

- Does not support applying a function (pre-compiled) as an argument yet.
- Several, more sophisticated optimizations are currently missing.
- No support for PlutusTx.

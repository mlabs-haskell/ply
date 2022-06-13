# Ply

Ply allows you to serialize your Plutarch validators/minting policies (with optionally extra parameters) into the file system, and read them back - preserving the type information.

This facilitates the onchain/offchain split that is often utilized, without forcing the user to manage the intricacies of the types and their UPLC representation when it comes to parameterized scripts. i.e scripts that take extra parameters before being submitted to the chain.

# Goals

- Efficiency: Applying constants with `Ply` should be equally efficient as using `pconstant`/`pconstantData` in the Plutarch function body directly.
- Ergonomics: Users shouldn't need to deal with UPLC intricacies.
- Invariant validation: Before given Haskell constant is applied to the script, all its invariants are validated and any relevant normalization is performed. e.g `PValue` is sorted and normalized (zero elements removed) before being passed into the script.

  You can bestow custom invariants for your custom types using `PlyArg`.
- First class Plutarch support: Preference given to and ease of use with Plutarch written scripts.
- Minimal dependencies: Not dependent on the entire `plutus-apps` stack, not dependent on problematic/conflicting dependencies that may prevent compilation in certain repositories.

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
  -- | Description for the file, semantically irrelevant - just for human comprehension!
  Text ->
  -- | The path where this file should be written
  FilePath ->
  -- | The Plutarch term itself, of course!
  ClosedTerm pt ->
  IO ()
```

> Aside: The `TypedWriter` constraint on the Plutarch type effectively barricades unsupported types. Of course, only validators and minting policies with 0 or more extra parameters are supported. The specific parameter types themselves also need to satisfy some constraints.

This is how it'd look in practice:

```hs
import Plutarch
import Plutarch.Builtin (pasInt)
import Plutarch.Prelude
import Plutarch.Api.V1

import Ply.Plutarch (writeTypedScript)

parameterizedLock :: ClosedTerm (PInteger :--> PData :--> PInteger :--> PScriptContext :--> PUnit)
parameterizedLock = plam $ \i datm redm ctx -> pif (redm #== i) (pconstant ()) $ ptraceError "incorrect guess"

parameterizedLockV :: ClosedTerm (PInteger :--> PValidator)
parameterizedLockV = plam $ \i datm redm ctx -> parameterizedLock v # datm # (pasInt # redm) # ctx

main :: IO ()
main =
  writeTypedScript "Parameterized lock validator" "path/to/script.plutus" parameterizedLockV
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

Here's a full example of obtaining the `Validator` from our `parameterizedLockV` by applying the integer `42` as the extra parameter:

```hs
import Data.Text (Text)

import Plutus.Contract (Contract, EmptySchema)
import Plutus.V1.Ledger.Scripts (Validator)

import Ply (readTypedScript, (#))
import qualified Ply

someContract :: Validator -> Contract () EmptySchema Text ()
someContract lockV = ... -- imagine a really useful contract!

runContract :: Contract w s e a -> IO a
runContract = ... -- imagine a contract runner impl!

main :: IO ()
main = do
  parameterizedLockV <- readTypedScript "path/to/script.plutus"
  runContract . someContract . Ply.toValidator $ parameterizedLockV # param
  where
    param :: Integer
    param = 42
```

> Aside: Notice how I didn't use type applications, it got inferred automatically!

# Shortcomings/Pitfalls

- No `PAsData` support yet. As a result, you can use, e.g `PCurrencySymbol` as an extra parameter, but not `PAsData PCurrencySymbol`.

  `PData` as an extra parameter _is_ still supported though.
- Does not support applying a function (pre-compiled) as an argument yet.
- Several, more sophisticated optimizations are currently missing.
- No support for PlutusTx.

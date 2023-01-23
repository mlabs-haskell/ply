# 0.4.1 - Mapping for Typename on loading

`readTypedScriptWith` allows one to load scripts, which have been serialized with an older version of `ply`
to be loaded, see README.md for example.

# 0.3.0 - Plutus update, Plutarch 1.3, API changes

There is no `Script` type in upstream Plutus anymore. As a result, `TypedScript` holds the UPLC program itself.

Note that `Script` from Plutus was previously only a newtype to the UPLC program anyway.

- Remove `toValidator`, `toMintingPolicy` (`Validator` and `MintingPolicy` no longer exist in Plutus)

  Instead, `TypedScript` is now exported as a pattern - extraction of the inner UPLC program is only allowed once
  all parameters are fully applied. See `example/reader-app`

  Once you have obtained the inner UPLC, you may use it in your own `Validator`/`MintingPolicy` types depending on the PAB.

- Remove several `PlyArg` and `PlyArgOf` instances (due to corresponding types being removed from upstream Plutus)
- `new-ledger-namespace` flag has been removed. Please consider _always_ using the new ledger namespace in all projects.

  Maintaining compat with the old ledger api, which exports certain types now removed from the newer ledger api, seems not worth
  the effort.
- `ply-core` and `ply-plutarch` are no longer built in different cabal environments. Heavy simplifications in haskell.nix and usage of CHaP allows both to be built in the same environment, without jeopardizing downstream projects (in case they still need a repo separation).

  Thanks to @MangoIV for help with nix!

# 0.2.0 - Internal Type Cleanup

- `TypedScriptEnvelope'` is deprecated. `TypedScriptEnvelope` now implements `toJSON` and `fromJSON` directly.
- `writeEnvelope` and `readEnvelope` are updated to have more sensible types. They will read and write to and from `TypedScriptEnvelope`.
- `mkEnvelope` at `Ply.Plutarch` will convert Plutarch term into `TypedScriptEnvelope` given config and description.
- `CBORDecodeError` is removed, `AesonDecodeError` will be thrown when decoding error happen.
- Script serialization utilites are now relocated to `Ply.Core.Serialize.Script`.

# 0.1.3 - **Critical fix** for `PlyArg Extended` instance

- The constructor indices were incorrect.

  Thanks to @danielfarrelly for reporting and fixing the bug.

# 0.1.2 - **Critical fix** for `PlyArg Credential` instance

- The instance was buggy and did not produce proper UPLC. Any types containing `Credential` (e.g `Address`) may have been affected.

  Thanks to @rmgaray for reporting the bug.

# 0.1.1 - Hot fix for `PlyArg`

- Export `ToDataConstraint` and `toBuiltinArgData`.

# 0.1.0 - Initial Release

- Full Plutus V2 support with machinery for distinguishing between V1 and V2 Plutarch scripts.
- Add `getPlutusVersion` - query a `TypedScript` for its Plutus version (`ScriptV1` or `ScriptV2`).
- Plutarch 1.2 integration
- New flag for `ply-core`: `new-ledger-namespace`, turned on by default. See README.
- `writeTypedScript` and `typedWriterInfo` are now parameterized over the Plutarch compiler config.
- More documentation in general, please consider checking out the haddocks locally.
- Fix `typeName` generation: this is somewhat of an internal detail and things will _just work_. But you may consider reading the README section on type name determination in case you're having strange issues during script reading.

## Acknowledgements

Huge thanks to these folks helping me:

- @bladyjoker
- @danielfarrelly
- @MangoIV

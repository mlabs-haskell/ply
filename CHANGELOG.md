# 0.6.0 - Bump plutus version to 1.7.0.0

# 0.5.0 - More detailed parameter type encoding that supports data encoded parameters.

- Add `PlyTypeName` and `plyTypeName` that replaces old `typeName` function. This provides mode detailed
  type reflections: type applications, typelits, and modules name. 
- Add `AsData`, an identity type that represents data encoded parameters. Parallels `PAsData` in Plutarch.
- Add proper instances for `AsData` to make it work with `plyTypeName` and `applyTerm`. 
- Add `unsafeUnTypedScript'`, same as `unsafeUnTypedScript` but using unboxed tuple.

- Due to the changes made in `Ply.Core.Typename` module, scripts exported after v0.5.0 __will not__ be compatible
  with scripts earlier versions of Ply. Users simply need to bump Ply version to make it compatible as there are
  no API changes made

# 0.4.0 -  Direct 'TypedScript' access and usage, and add Rational instances

- Add `Ply.Core.Unsafe` to the public API, which allows direct access to `TypedScript`.
- Add `Ply.Plutarch.toTypedScript` to obtain a `TypedScript` from Plutarch term directly.

  See README for the motivation.
- Add `Ply.Core.TypedReader.typedScriptToEnvelope` to obtain a `TypedScriptEnvelope` from `TypedScript`.
- Add `PlyArg` and `PlyArgOf` instance for `PlutusTx.Ratio.Rational` and `Plutarch.Extra.PRationalData.PRationalData`.

# 0.3.1 - Hotfix for Integer parameters

- Older GHCs export 'Integer' at "GHC.Num.Integer", but older ones have it at "GHC.Integer.Type". So two GHC version discrepancy between onchain and offchain where one of the typed scripts used integers broke everything.

  Ply will now rename the old module (if encountered) to be like the new one.

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

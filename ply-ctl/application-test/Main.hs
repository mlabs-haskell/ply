module Main (main) where

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import Data.Default (def)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Plutarch.Api.V1
import Plutarch.Prelude
import PlutusLedgerApi.V1
import Ply
import Ply.Core.Unsafe (unsafeUnTypedScript')
import Ply.Plutarch
import System.Exit
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck as QC

sampleScriptA ::
  ClosedTerm
    ( PAsData PInteger
        :--> PMintingPolicy
    )
sampleScriptA =
  plam $ \_ _ _ ->
    popaque $ pconstant ()

sampleScriptB ::
  ClosedTerm
    ( PAsData PInteger
        :--> PAsData PTxOutRef
        :--> PMintingPolicy
    )
sampleScriptB =
  plam $ \_ _ _ _ ->
    popaque $ pconstant ()

propertyA ::
  TypedScript 'MintingPolicyRole '[AsData Integer] ->
  Integer ->
  Property
propertyA scr x = monadicIO $ do
  (code, purs, stderr) <-
    run $
      readProcessWithExitCode
        "spago"
        [ "run"
        , "--main"
        , "ApplicationTest"
        , "-a"
        , show x
        ]
        ""

  let haskell =
        Text.decodeUtf8
          . Base16.encode
          . SBS.fromShort
          . serialiseUPLC
          . snd
          . unsafeUnTypedScript'
          $ scr #! AsData x

  case code of
    ExitSuccess ->
      pure $
        property $
          counterexample
            ( "haskell:\n"
                <> Text.unpack haskell
                <> "\npurescript:\n"
                <> purs
            )
            $ purs == Text.unpack haskell
    ExitFailure c ->
      pure $
        counterexample
          ( "Purescript process failed with "
              <> show c
              <> "\n"
              <> stderr
          )
          False

main :: IO ()
main = do
  writeTypedScript
    def
    "SampleA"
    ("application-test" </> "compiled" </> "SampleA.plutus")
    sampleScriptA

  writeTypedScript
    def
    "SampleB"
    ("application-test" </> "compiled" </> "SampleB.plutus")
    sampleScriptB

  sampleA :: TypedScript 'MintingPolicyRole '[AsData Integer] <-
    readTypedScript
      ("application-test" </> "compiled" </> "SampleA.plutus")

  _sampleB :: TypedScript 'MintingPolicyRole '[AsData Integer, AsData TxOutRef] <-
    readTypedScript
      ("application-test" </> "compiled" </> "SampleB.plutus")

  defaultMain . adjustOption go $
    testGroup
      "Tests"
      [ QC.testProperty "Applying Integer" $ propertyA sampleA
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = const 10

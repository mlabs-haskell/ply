{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Type (MyParameter (..)) where

import GHC.Generics (Generic)

import PlutusLedgerApi.Data.V3 (TokenName, TxOutRef)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)

{- | The corresponding Haskell type for our script's parameter, following Plutarch guidelines.
 We can apply this type to the script on the reader side.
-}
data MyParameter = MyParameter {myParameter'tn :: TokenName, myParameter'ref :: TxOutRef}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Make sure to use PlutusTx's tools for implementing data and blueprint instances for our custom type!
$(makeIsDataSchemaIndexed ''MyParameter [('MyParameter, 0)])

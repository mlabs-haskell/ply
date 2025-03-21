{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.Type (MyParameter (..)) where

import GHC.Generics (Generic)

import PlutusLedgerApi.Data.V3 (TokenName, TxOutRef)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import Ply (PlyArg)

{- | The corresponding Haskell type for our script's parameter, following Plutarch guidelines.
 We can apply this type to the script on the reader side.
-}
data MyParameter = MyParameter {myParameter'tn :: TokenName, myParameter'ref :: TxOutRef}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

-- Make sure to use PlutusTx's tools for implementing data and blueprint instances for our custom type!
$(makeIsDataSchemaIndexed ''MyParameter [('MyParameter, 0)])

-- PlyArg can be default (i.e 'anyclass') derived for 'MyParameter' since it's a data encoded type and
-- its data encoding is also default derived (PlutusTx).
-- Note that we were unable to put 'PlyArg' into the 'deriving anyclass' above (attached to the datatype definition) due to
-- the TH derivation. 'PlyArg' requires certain typeclasses which are only recognized as existing _after_ the 'makeIsDataSchemaIndexed' expansion.
instance PlyArg MyParameter

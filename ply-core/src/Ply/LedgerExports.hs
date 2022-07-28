{-# LANGUAGE CPP #-}

module Ply.LedgerExports (
  module LedgerV1,
) where

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V1 as LedgerV1
import PlutusLedgerApi.V1.Scripts as LedgerV1 (
  Script (Script),
  ScriptHash (ScriptHash),
 )
import PlutusLedgerApi.V1.Time as LedgerV1 (
  DiffMilliSeconds (DiffMilliSeconds)
 )
import PlutusLedgerApi.V1.Value as LedgerV1 (
  AssetClass (AssetClass)
 )
#else
import Plutus.V1.Ledger.Api as LedgerV1
import Plutus.V1.Ledger.Scripts as LedgerV1 (
  Script (Script),
  ScriptHash (ScriptHash),
 )
import Plutus.V1.Ledger.Time as LedgerV1 (
  DiffMilliSeconds (DiffMilliSeconds)
 )
import Plutus.V1.Ledger.Value as LedgerV1 (
  AssetClass (AssetClass)
 )
#endif

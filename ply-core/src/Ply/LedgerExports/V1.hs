{-# LANGUAGE CPP #-}

module Ply.LedgerExports.V1 (
  module LedgerV1,
) where

#ifdef NEW_LEDGER_NAMESPACE
import PlutusLedgerApi.V1 as LedgerV1
#else
import Plutus.V1.Ledger.Api as LedgerV1
#endif

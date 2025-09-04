-- Small Plutus / Cardano example
-- Author: Olivier Rishi Matabaro

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MiniCardano where

import PlutusTx.Prelude
import Plutus.V2.Ledger.Api

-- A minimal validator: always True
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True

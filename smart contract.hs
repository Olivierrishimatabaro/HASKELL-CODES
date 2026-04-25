{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}

module VestingContract where

import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Contexts
import           Ledger.TimeSlot
import           Prelude (Show)
import           GHC.Generics (Generic)

-- | Données du contrat
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
    deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

-- | Redeemer (pas utilisé ici mais requis)
data VestingRedeemer = Claim
PlutusTx.unstableMakeIsData ''VestingRedeemer

-- | Fonction principale de validation
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> VestingRedeemer -> ScriptContext -> Bool
mkValidator datum _ ctx =
    traceIfFalse "Not signed by beneficiary" signedByBeneficiary &&
    traceIfFalse "Deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Vérifie la signature
    signedByBeneficiary :: Bool
    signedByBeneficiary =
        txSignedBy info (beneficiary datum)

    -- Vérifie la date
    deadlineReached :: Bool
    deadlineReached =
        contains (from $ deadline datum) (txInfoValidRange info)

-- | Compilation du validator
{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    check $
        mkValidator
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

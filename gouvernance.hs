{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CardanoGovernance where

import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Typed.Scripts

-- Représente un vote
data VoteChoice = Yes | No | Abstain
PlutusTx.unstableMakeIsData ''VoteChoice

-- Proposition de gouvernance
data Proposal = Proposal
    { proposalId   :: Integer
    , titleHash    :: BuiltinByteString
    , deadline     :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''Proposal

-- Redeemer = choix du vote
newtype VoteRedeemer = VoteRedeemer VoteChoice
PlutusTx.unstableMakeIsData ''VoteRedeemer

-- Vérifie si le vote est encore ouvert
{-# INLINABLE mkValidator #-}
mkValidator :: Proposal -> () -> VoteRedeemer -> ScriptContext -> Bool
mkValidator proposal _ _ ctx =
    traceIfFalse "Voting closed" beforeDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        contains (to $ deadline proposal) (txInfoValidRange info)

data Voting
instance Scripts.ValidatorTypes Voting where
    type instance DatumType Voting = Proposal
    type instance RedeemerType Voting = VoteRedeemer

typedValidator :: Scripts.TypedValidator Voting
typedValidator =
    Scripts.mkTypedValidator @Voting
        $$(compile [|| mkValidator ||])
        $$(compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

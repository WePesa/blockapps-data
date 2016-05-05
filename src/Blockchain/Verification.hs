{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Verification (
  transactionsVerificationValue,
  ommersVerificationValue,
  receiptsVerificationValue
  ) where

import Prelude.Unicode

import Blockchain.Data.BlockDB
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.SHA



transactionsVerificationValue::[Transaction]->MP.StateRoot
transactionsVerificationValue = MP.sha2StateRoot . listToRLPVerificationValue

ommersVerificationValue::[BlockData]->SHA
ommersVerificationValue = listToRLPVerificationValue 

receiptsVerificationValue::()->MP.StateRoot
receiptsVerificationValue _ = MP.emptyTriePtr

listToRLPVerificationValue :: (RLPSerializable a) => [a] -> SHA
listToRLPVerificationValue = hash ∘ rlpSerialize ∘ RLPArray ∘ map rlpEncode

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



transactionsVerificationValue::[Transaction]->MP.SHAPtr
transactionsVerificationValue = MP.sha2SHAPtr . listToRLPVerificationValue

ommersVerificationValue::[BlockData]->SHA
ommersVerificationValue = listToRLPVerificationValue 

receiptsVerificationValue::()->MP.SHAPtr
receiptsVerificationValue _ = MP.emptyTriePtr

listToRLPVerificationValue :: (RLPSerializable a) => [a] -> SHA
listToRLPVerificationValue = hash ∘ rlpSerialize ∘ RLPArray ∘ map rlpEncode

{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Verification (
  transactionsVerificationValue,
  ommersVerificationValue,
  receiptsVerificationValue
  ) where

import Prelude.Unicode

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.SHA



transactionsVerificationValue::[Transaction]->MP.SHAPtr
transactionsVerificationValue _ = MP.emptyTriePtr

ommersVerificationValue::[BlockData]->SHA
ommersVerificationValue = hash ∘ rlpSerialize ∘ RLPArray ∘  map rlpEncode

receiptsVerificationValue::()->MP.SHAPtr
receiptsVerificationValue _ = MP.emptyTriePtr

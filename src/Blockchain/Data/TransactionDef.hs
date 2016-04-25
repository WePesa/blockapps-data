{-# LANGUAGE DeriveGeneric #-}

module Blockchain.Data.TransactionDef (
  Transaction(..)
  ) where

import qualified Data.ByteString as B
import Data.Word
import Database.Persist
import Database.Persist.TH
import GHC.Generics

import Blockchain.Data.Address
import Blockchain.ExtWord
import Blockchain.Data.Code

derivePersistField "Transaction"

data Transaction = 
  MessageTX {
    transactionNonce::Integer,
    transactionGasPrice::Integer,
    transactionGasLimit::Integer,
    transactionTo::Address,
    transactionValue::Integer,
    transactionData::B.ByteString,
    transactionR::Integer,
    transactionS::Integer,
    transactionV::Word8
   } |
  ContractCreationTX {
    transactionNonce::Integer,
    transactionGasPrice::Integer,
    transactionGasLimit::Integer,
    transactionValue::Integer,
    transactionInit::Code,
    transactionR::Integer,
    transactionS::Integer,
    transactionV::Word8
    } deriving (Show, Read, Eq, Ord, Generic)



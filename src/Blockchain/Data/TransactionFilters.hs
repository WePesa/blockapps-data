{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.TransactionFilters where

import qualified Database.Esqueleto as E

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString.Base16 as B16
import Database.Persist

import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.DataDefs
import Blockchain.Data.AccountFilters (toAddr)

transactionQueryParams:: [T.Text]
transactionQueryParams = [ "address",
                           "from",
                           "to",
                           "hash",
                           "gasprice",
                           "mingasprice",
                           "maxgasprice",
                           "gaslimit",
                           "mingaslimit",
                           "maxgaslimit",
                           "value",
                           "minvalue", 
                           "maxvalue",
                           "blocknumber" ]

getTransFilter :: (E.Esqueleto query expr backend) => (expr (Entity RawTransaction))-> (T.Text, T.Text) -> expr (E.Value Bool)
getTransFilter  _          ("page", _)         = E.val True
getTransFilter  _          ("index", _)        = E.val True
getTransFilter  _          ("raw", _)          = E.val True
getTransFilter  _          ("next", _)         = E.val True
getTransFilter  _          ("prev", _)         = E.val True
getTransFilter  _          ("appname", _)      = E.val True
getTransFilter  _          ("sortby", _)       = E.val True

getTransFilter (rawTx)     ("address", v)      = rawTx E.^. RawTransactionFromAddress E.==. E.val (toAddr v) E.||. rawTx E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v))
getTransFilter (rawTx)     ("from", v)         = rawTx E.^. RawTransactionFromAddress E.==. E.val (toAddr v)
getTransFilter (rawTx)     ("to", v)           = rawTx E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v))
getTransFilter (rawTx)     ("hash", v)         = rawTx E.^. RawTransactionTxHash  E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) 

--getTransFilter (rawTx)     ("type", "Contract") = (rawTx E.^. RawTransactionToAddress E.==. (E.val "")) E.&&. (RawTransactionCodeOrData E.!=. (E.val ""))

getTransFilter (rawTx)     ("gasprice", v)     = rawTx E.^. RawTransactionGasPrice E.==. E.val (read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("mingasprice", v)  = rawTx E.^. RawTransactionGasPrice E.>=. E.val (read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxgasprice", v)  = rawTx E.^. RawTransactionGasPrice E.<=. E.val (read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("gaslimit", v)     = rawTx E.^. RawTransactionGasLimit E.==. E.val (read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("mingaslimit", v)  = rawTx E.^. RawTransactionGasLimit E.>=. E.val (read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxgaslimit", v)  = rawTx E.^. RawTransactionGasLimit E.<=. E.val (read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("value", v)        = rawTx E.^. RawTransactionValue E.==. E.val (read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("minvalue", v)     = rawTx E.^. RawTransactionValue E.>=. E.val (read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxvalue", v)     = rawTx E.^. RawTransactionValue E.<=. E.val (read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("blocknumber", v)  = rawTx E.^. RawTransactionBlockNumber E.==. E.val (read $ T.unpack v :: Int)
getTransFilter _           _                   = undefined ("no match in getTransFilter"::String)

getTxNum :: RawTransaction -> Int
getTxNum (RawTransaction _ _ _ _ _ _ _ _ _ _ _ bn _ _) = bn

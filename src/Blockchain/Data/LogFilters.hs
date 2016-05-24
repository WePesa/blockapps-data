{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.LogFilters where

import qualified Database.Esqueleto as E

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString.Base16 as B16

import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.DataDefs
import Blockchain.Data.AccountFilters (toAddr)

getLogFilter :: (E.Esqueleto query expr backend) => expr (E.Entity LogDB) -> (T.Text, T.Text) -> expr (E.Value Bool)
getLogFilter _ ("index",_) = E.val True         -- indexes are intercepted in handlers. We should probably deal with them here in the future
getLogFilter log' ("address",v) = log' E.^. LogDBAddress E.==. E.val (toAddr v)  
getLogFilter log' ("hash",v) = log' E.^. LogDBTransactionHash  E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) 
getLogFilter _           _  = undefined ("no match in getLogFilter"::String)


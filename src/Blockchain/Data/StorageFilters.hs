{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.StorageFilters where

import qualified Database.Esqueleto as E

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Binary as Bin
import qualified Data.ByteString.Lazy as BS

import Blockchain.ExtWord
import Blockchain.Data.DataDefs
import Blockchain.Data.AccountFilters (toAddr,toAddrId)
import Blockchain.Data.Params

getStorageFilter :: (E.Esqueleto query expr backend) => (expr (E.Entity Storage), expr (E.Entity AddressStateRef)) -> (T.Text, T.Text) -> expr (E.Value Bool)
getStorageFilter _ ("page",_)  = E.val True
getStorageFilter _ ("index",_) = E.val True
getStorageFilter (storage,_) ("key", v)
  = storage E.^. StorageKey E.==. E.val (fromIntegral (read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("minkey", v)
  = storage E.^. StorageKey E.>=. E.val (fromIntegral (read $ T.unpack v :: Integer) :: Word256) 
getStorageFilter (storage,_) ("maxkey", v)
  = storage E.^. StorageKey E.<=. E.val (fromIntegral (read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("keystring", v)
  = storage E.^. StorageKey E.==. E.val (Bin.decode $ BS.fromStrict $ T.encodeUtf8 v :: Word256)
getStorageFilter (storage,_) ("keyhex", v)
  = storage E.^. StorageKey E.==. E.val (fromHexText v)
getStorageFilter (storage,_) ("value", v)
  = storage E.^. StorageValue E.==. E.val (fromIntegral (read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("minvalue", v)
  = storage E.^. StorageValue E.>=. E.val (fromIntegral (read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("maxvalue", v)
  = storage E.^. StorageValue E.<=. E.val (fromIntegral (read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("valuestring", v)
  = storage E.^. StorageValue E.==. E.val (Bin.decode $ BS.fromStrict $ T.encodeUtf8 v :: Word256)
getStorageFilter (storage,_) ("addressid", v)
  = storage E.^. StorageAddressStateRefId E.==. E.val (toAddrId v)
getStorageFilter (_,addrStRef) ("address", v)      -- Note: a join is done in StorageInfo
  = addrStRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)  
getStorageFilter _           _                   = undefined ("no match in getStorageFilter"::String)


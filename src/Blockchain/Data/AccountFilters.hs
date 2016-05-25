{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.AccountFilters where

import qualified Database.Esqueleto as E

import qualified Data.Text as T

import Numeric

import Blockchain.ExtWord
import Blockchain.Data.DataDefs
import Blockchain.Data.Address
import Blockchain.Data.Params

accountQueryParams:: [T.Text]
accountQueryParams = [ "address",
                       "balance",
                       "minbalance",
                       "maxbalance",
                       "nonce",
                       "minnonce",
                       "maxnonce", 
                       "maxnumber" ]


getAccFilter :: (E.Esqueleto query expr backend) => (expr (E.Entity AddressStateRef))-> (T.Text, T.Text) -> expr (E.Value Bool)
getAccFilter  _            ("page", _)         =  E.val True
getAccFilter  _            ("index", _)        =  E.val True
getAccFilter  _            ("raw", _)          =  E.val True
getAccFilter  _            ("next", _)         =  E.val True
getAccFilter  _            ("prev", _)         =  E.val True
getAccFilter  _            ("appname", _)      =  E.val True

getAccFilter (accStateRef) ("balance", v)      = accStateRef E.^. AddressStateRefBalance E.==. E.val (read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("minbalance", v)   = accStateRef E.^. AddressStateRefBalance E.>=. E.val (read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("maxbalance", v)   = accStateRef E.^. AddressStateRefBalance E.<=. E.val (read $ T.unpack v :: Integer) 

getAccFilter (accStateRef) ("nonce", v)        = accStateRef E.^. AddressStateRefNonce E.==. E.val (read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("minnonce", v)     = accStateRef E.^. AddressStateRefNonce E.>=. E.val (read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("maxnonce", v)     = accStateRef E.^. AddressStateRefNonce E.<=. E.val (read $ T.unpack v :: Integer)

getAccFilter (accStateRef) ("address", v)      = accStateRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)
getAccFilter _             _                   = undefined ("no match in getAccFilter"::String)

toAddrId :: T.Text -> E.Key AddressStateRef
toAddrId = toId

toAddr :: T.Text -> Address
toAddr v = Address wd160
  where ((wd160, _):_) = readHex $ T.unpack $ v :: [(Word160,String)]

-- probably need to pad here
getAccNum :: AddressStateRef -> String
getAccNum (AddressStateRef (Address x) _ _ _ _ _) = (showHex x "")


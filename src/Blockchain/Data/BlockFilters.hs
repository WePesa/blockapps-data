{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.BlockFilters where

import qualified Database.Esqueleto as E

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as B16

import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.DataDefs
import Blockchain.Data.Address
import Blockchain.Data.Params

import Blockchain.Data.AccountFilters (toAddr) 

blockQueryParams:: [T.Text]
blockQueryParams = [ "txaddress",
                     "coinbase",
                     "address",
                     "blockid",
                     "hash",
                     "mindiff",
                     "maxdiff",
                     "diff",
                     "gasused",
                     "mingasused",
                     "maxgasused",
                     "gaslim",
                     "mingaslim",
                     "maxgaslim",
                     "number",
                     "minnumber", 
                     "maxnumber" ]


getBlkFilter :: (E.Esqueleto query expr backend) => (expr (E.Entity BlockDataRef), expr (E.Entity AddressStateRef), expr (E.Entity RawTransaction), expr (E.Entity Block))-> (T.Text, T.Text) -> expr (E.Value Bool)

getBlkFilter  _                               ("page", _)    = E.val True 
getBlkFilter  _                               ("index", _)    = E.val True 
getBlkFilter  _                               ("raw", _)    = E.val True 
getBlkFilter  _                               ("next", _)    = E.val True
getBlkFilter  _                               ("prev", _)    = E.val True
getBlkFilter  _                               ("appname", _) = E.val True
getBlkFilter (bdRef, _, _, _)                 ("ntx", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (read $ T.unpack v :: Integer)

getBlkFilter (bdRef, _, _, _)                 ("number", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (read $ T.unpack v :: Integer)
getBlkFilter (bdRef, _, _, _)                 ("minnumber", v)    = bdRef E.^. BlockDataRefNumber E.>=. E.val (read $ T.unpack v :: Integer)
getBlkFilter (bdRef, _, _, _)                 ("maxnumber", v)    = bdRef E.^. BlockDataRefNumber E.<=. E.val (read $ T.unpack v :: Integer)

getBlkFilter (bdRef, _, _, _)                 ("gaslim", v)    = bdRef E.^. BlockDataRefGasLimit E.==. E.val (read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mingaslim", v) = bdRef E.^. BlockDataRefGasLimit E.>=. E.val (read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxgaslim", v) = bdRef E.^. BlockDataRefGasLimit E.<=. E.val (read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, _, _, _)                 ("gasused", v)    = bdRef E.^. BlockDataRefGasUsed E.==. E.val (read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mingasused", v) = bdRef E.^. BlockDataRefGasUsed E.>=. E.val (read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxgasused", v) = bdRef E.^. BlockDataRefGasUsed E.<=. E.val (read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, _, _, _)                 ("diff", v)      = bdRef E.^. BlockDataRefDifficulty E.==. E.val (read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mindiff", v)   = bdRef E.^. BlockDataRefDifficulty E.>=. E.val (read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxdiff", v)   = bdRef E.^. BlockDataRefDifficulty E.<=. E.val (read $ T.unpack v :: Integer) 

-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("time", v)      = bdRef E.^. BlockDataRefTimestamp E.==. E.val (stringToDate v)
-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mintime", v)   = bdRef E.^. BlockDataRefTimestamp E.>=. E.val (stringToDate v)
-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxtime", v)   = bdRef E.^. BlockDataRefTimestamp E.<=. E.val (stringToDate v)

getBlkFilter (_, _, rawTX, _) ("txaddress", v) = (rawTX E.^. RawTransactionFromAddress E.==. E.val (toAddr v))
                                                 E.||. (rawTX E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v)))

getBlkFilter (bdRef, _, _, blk) ("coinbase", v)  = bdRef E.^. BlockDataRefCoinbase E.==. E.val (toAddr v)
                                                              E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
getBlkFilter (_, accStateRef, _, _) ("address", v)   = accStateRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)
getBlkFilter (bdRef, _, _, blk) ("blockid", v)   = bdRef E.^. BlockDataRefBlockId E.==. E.val (toBlockId v)
                                                              E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
getBlkFilter (bdRef, _, _, blk) ("hash", v)   = (bdRef E.^. BlockDataRefHash E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) ) E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
                            

getBlkFilter _ _ = undefined ("no match in getBlkFilter"::String)

toBlockId :: T.Text -> E.Key Block
toBlockId = toId

getBlockNum :: Block -> Integer
getBlockNum (Block (BlockData _ _ (Address _) _ _ _ _ _ num _ _ _ _ _ _) _ _) = num 

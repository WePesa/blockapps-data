{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module Blockchain.Data.NewBlk (
  NewBlk(..),
  getNewBlk,
  putNewBlk,
  blockToNewBlk,
  newBlkToBlock
) where 

import Control.Lens

import Control.Exception.Lifted

import Database.Persist hiding (get)
import qualified Database.Persist.Postgresql as SQL
import qualified Database.Esqueleto as E

import Data.Bits
import qualified Data.ByteString as B

import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Network.Kafka
import Network.Kafka.Producer
import Network.Kafka.Protocol hiding (Key)

import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Constants
import Blockchain.Data.Address
import Blockchain.Data.BlockHeader
import qualified Blockchain.Colors as CL

import Blockchain.DB.SQLDB

import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.DB.KafkaTools
import Blockchain.Data.BlockDB
import Blockchain.Data.RLP
import Blockchain.Data.BlockOffset
import Blockchain.SHA
import Blockchain.Util
import Blockchain.Data.RawTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.DataDefs
import Blockchain.Data.Code

import Control.Monad.State
import Control.Monad.Trans.Resource

--import Debug.Trace

getNewBlk::(HasSQLDB m, MonadResource m, MonadBaseControl IO m)=>
          SHA->m (Maybe NewBlk)
getNewBlk h = do
  db <- getSQLDB
  res <- runResourceT $
    SQL.runSqlPool (SQL.getBy $ TheHash h) db

  return $ fmap entityVal res

putNewBlk::(HasSQLDB m, MonadResource m, MonadBaseControl IO m, MonadThrow m)=>
           NewBlk->m ()
putNewBlk blk = do
  db <- getSQLDB
  (x::Either SomeException (Key NewBlk)) <- try $ runResourceT $
                                  flip SQL.runSqlPool db $
                                  SQL.insert blk

  return ()

blockToNewBlk::Block->NewBlk
blockToNewBlk b@Block{blockBlockData=bd,blockReceiptTransactions=t,blockBlockUncles=u} =
  NewBlk {
    newBlkHash=blockHash b,
    newBlkBlockData=bd,
    newBlkReceiptTransactions=t,
    newBlkBlockUncles=u
    }

newBlkToBlock::NewBlk->Block
newBlkToBlock NewBlk{newBlkBlockData=bd,newBlkReceiptTransactions=t,newBlkBlockUncles=u} =
  Block {
    blockBlockData=bd,
    blockReceiptTransactions=t,
    blockBlockUncles=u
    }

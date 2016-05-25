{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Blockchain.Data.LogQueries where

import qualified Database.Esqueleto as E 

import qualified Data.Text as T 

import Control.Monad.Trans.Resource

import Blockchain.DB.SQLDB
import Blockchain.Data.DataDefs
import Blockchain.Data.LogFilters

import GHC.Int

type LogIndex = Int64
type LogFetchLimit = Int64

genLogQuery :: (HasSQLDB m, MonadResource m, MonadBaseControl IO m) 
            => [(T.Text,T.Text)]
            -> LogIndex
            -> LogFetchLimit 
            -> m [E.Entity LogDB]
genLogQuery params index' limit = do
   sqlQuery $ 
       E.select $ 
           E.from $ \(lg) -> do

           let criteria = map (getLogFilter lg) $ params
               allCriteria = ((lg E.^. LogDBId) E.>=. E.val (E.toSqlKey index')) : criteria

           E.where_ (foldl1 (E.&&.) allCriteria)

           E.limit $ limit

           E.orderBy [E.desc (lg E.^. LogDBId)]

           return lg
    


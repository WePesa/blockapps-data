
module Blockchain.Data.Extra (
     getBestBlockInfo,
     putBestBlockInfo,
    ) where

import qualified Database.Persist.Sql as SQL

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB

getBestBlockInfo::HasSQLDB m =>
                  m (MP.SHAPtr, Integer)
getBestBlockInfo = 
  fmap (read . extraValue) $ sqlQuery $ SQL.getJust (ExtraKey "bestBlock")

putBestBlockInfo::HasSQLDB m=>
                MP.SHAPtr->Integer->m ()
putBestBlockInfo sr bestNumber = do
  _ <- sqlQuery $ SQL.upsert (Extra "bestBlock" $ show (sr, bestNumber)) []
  return ()

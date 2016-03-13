
module Blockchain.Data.Extra (
     getBestBlockInfo,
     putBestBlockInfo,
    ) where

import qualified Database.Persist.Sql as SQL

import Blockchain.Data.DataDefs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.SQLDB
import Blockchain.SHA

getBestBlockInfo::HasSQLDB m =>
                  m (SHA, BlockData)
getBestBlockInfo = 
  fmap (read . extraValue) $ sqlQuery $ SQL.getJust (ExtraKey "bestBlock")

putBestBlockInfo::HasSQLDB m=>
                SHA->BlockData->m ()
putBestBlockInfo hash bd = do
  _ <- sqlQuery $ SQL.upsert (Extra "bestBlock" $ show (hash, bd)) []
  return ()

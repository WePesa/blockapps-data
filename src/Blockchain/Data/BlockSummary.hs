{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Blockchain.Data.BlockSummary (
    BlockSummary(..),
    blockToBSum
  ) where

import Control.Monad
--import Control.Monad.Base
import Control.Monad.Trans
--import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
    
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.Format
import Blockchain.SHA
                
data BlockSummary = BlockSummary {
  bSumParentHash::SHA,
  bSumDifficulty::Integer,
  bSumTotalDifficulty::Int,
  bSumStateRoot::MP.StateRoot,
  bSumGasLimit::Integer,
  bSumTimestamp::UTCTime,
  bSumNumber::Integer
  }

blockToBSum::Block->BlockSummary
blockToBSum b = 
    BlockSummary {
      bSumParentHash = blockDataParentHash $ blockBlockData b,
      bSumDifficulty = blockDataDifficulty $ blockBlockData b,
      bSumTotalDifficulty = 0, -- blockDataTotalDifficulty $ blockBlockData b,
      bSumStateRoot = blockDataStateRoot $ blockBlockData b,
      bSumGasLimit = blockDataGasLimit $ blockBlockData b,
      bSumTimestamp = blockDataTimestamp $ blockBlockData b,
      bSumNumber = blockDataNumber $ blockBlockData b
    }

instance RLPSerializable BlockSummary where
  rlpEncode (BlockSummary p d td sr gl ts n) =
    RLPArray [
      rlpEncode p,
      rlpEncode d,
      rlpEncode $ toInteger td,
      rlpEncode sr,
      rlpEncode gl,
      rlpEncode (round $ utcTimeToPOSIXSeconds ts::Integer),
      rlpEncode n
      ]
  rlpDecode (RLPArray [p, d, td, sr, gl, ts, n]) =
    BlockSummary {
      bSumParentHash = rlpDecode p,
      bSumDifficulty = rlpDecode d,
      bSumTotalDifficulty = fromInteger $ rlpDecode td,
      bSumStateRoot = rlpDecode sr,
      bSumGasLimit = rlpDecode gl,
      bSumTimestamp = posixSecondsToUTCTime $ fromInteger $ rlpDecode ts,
      bSumNumber = rlpDecode n
      }
  rlpDecode x = error $ "rlpDecode for BlockSummary called with data of wrong format: " ++ show x

          

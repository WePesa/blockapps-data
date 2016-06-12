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


module Blockchain.Stream.VMEvent (
  VMEvent(..),
  produceVMEvents,
  fetchVMEvents,
  fetchVMEventsIO,
  fetchVMEventsOneIO,
  fetchLastVMEvents,
  getBestKafkaBlockNumber
) where 

import Control.Lens

import Control.Exception.Lifted

import qualified Data.ByteString as B

import Network.Kafka
import Network.Kafka.Producer
import Network.Kafka.Protocol hiding (Key)

import Blockchain.DB.SQLDB

import Blockchain.Format
import Blockchain.Stream.Raw
import Blockchain.Data.BlockOffset
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.KafkaTopics
import Blockchain.EthConf

import Control.Monad.State

--import Debug.Trace


data VMEvent = ChainBlock Block | NewUnminedBlockAvailable

instance Format VMEvent where
  format (ChainBlock b) = "Block: " ++ format b
  format NewUnminedBlockAvailable = "<NewUnminedBlockAvailable>"
  
bytesToVMEvent::B.ByteString->VMEvent
bytesToVMEvent b | B.head b == 0 = ChainBlock $ rlpDecode $ rlpDeserialize $ B.tail b
bytesToVMEvent b | B.head b == 1 = NewUnminedBlockAvailable
bytesToVMEvent _ = error "VMEvent has unsupported first byte"

vmEventToBytes::VMEvent->B.ByteString
vmEventToBytes (ChainBlock b) = 0 `B.cons` rlpSerialize (rlpEncode b)
vmEventToBytes NewUnminedBlockAvailable = B.singleton 1

produceVMEvents::(HasSQLDB m, MonadIO m)=>[VMEvent]->m Offset
produceVMEvents vmEvents = do
  result <- liftIO $ runKafkaConfigured "blockapps-data" $
            produceMessages $ map (TopicAndMessage (lookupTopic "block") . makeMessage . vmEventToBytes) vmEvents

  case result of
   Left e -> error $ show e
   Right x -> do
     let [offset] = concat $ map (map (\(_, _, x') ->x') . concat . map snd . _produceResponseFields) x
         newBlocks = [b | ChainBlock b <- vmEvents]
     (_::Either SomeException ()) <- try $ putBlockOffsets $ map (\(b, o) -> BlockOffset (fromIntegral o) (blockDataNumber $ blockBlockData b) (blockHash b)) $ zip newBlocks [offset..]
     return offset

fetchVMEvents::Offset->Kafka [VMEvent]
fetchVMEvents = fmap (map bytesToVMEvent) . fetchBytes (lookupTopic "block")

fetchVMEventsIO::Offset->IO (Maybe [VMEvent])
fetchVMEventsIO offset = do
  fmap (fmap (map bytesToVMEvent)) $ fetchBytesIO (lookupTopic "block") offset

fetchVMEventsOneIO::Offset->IO (Maybe VMEvent)
fetchVMEventsOneIO offset = do
  fmap (fmap bytesToVMEvent) $ fetchBytesOneIO (lookupTopic "block") offset

fetchLastVMEvents::Offset->IO [VMEvent]
fetchLastVMEvents n = do
  ret <-
    runKafkaConfigured "strato-p2p-client" $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 (lookupTopic "block")
      when (lastOffset == 0) $ error "Block stream is empty, you need to run strato-setup to insert the genesis block."
      let offset = max (lastOffset - n) 0
      fetchVMEvents offset

  case ret of
    Left e -> error $ show e
    Right v -> return v

getBestKafkaBlockNumber::Offset->IO Integer
getBestKafkaBlockNumber n = do 
  initVMEventsErr <-
    runKafkaConfigured "strato-p2p-client" $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 (lookupTopic "block")
      when (lastOffset == 0) $ error "Block stream is empty, you need to run strato-setup to insert the genesis block."

      fetchVMEvents (max (lastOffset - n) 0)

  case initVMEventsErr of
    Left e -> error $ show e
    Right initVMEvents -> do
      let blocks = [ b | ChainBlock b <- initVMEvents ]
      case blocks of
        [] -> getBestKafkaBlockNumber (n - (fromIntegral $ length initVMEvents))
        xs -> return $ maximum (map (blockDataNumber . blockBlockData) xs)
    

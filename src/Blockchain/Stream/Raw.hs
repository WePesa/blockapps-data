{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Stream.Raw (
  fetchBytes,
  fetchBytesIO,
  fetchBytesOneIO
  ) where

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Maybe

import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Protocol hiding (Message)

import Blockchain.KafkaTopics
import Blockchain.EthConf

fetchBytes::TopicName->Offset->Kafka [B.ByteString]
fetchBytes topic offset = do
  result <- fetch (Offset $ fromIntegral offset) 0 topic
  return $ map tamPayload $ fetchMessages result

fetchBytesIO::TopicName->Offset->IO (Maybe [B.ByteString])
fetchBytesIO topic offset = do
  ret <-
      runKafkaConfigured "blockapps-data" $ do
      lastOffset <- getLastOffset LatestTime 0 topic

      if (offset > lastOffset)
        then return Nothing
        else do
          stateRequiredAcks .= -1
          stateWaitSize .= 1
          stateWaitTime .= 100000
          fmap (Just . map tamPayload . fetchMessages) $ fetch offset 0 topic


  case ret of
   Left e -> error $ show e
   Right v -> return v
              
fetchBytesOneIO::TopicName->Offset->IO (Maybe B.ByteString)
fetchBytesOneIO topic offset = do
  res <- fetchBytesIO topic offset
  case res of
   Nothing -> return Nothing
   Just (x:_) -> return $ Just x
   Just [] -> error "something impossible happened in fetchBytesOneIO"              

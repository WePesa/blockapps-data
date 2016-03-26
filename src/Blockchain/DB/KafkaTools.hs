{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.KafkaTools (
  fetchBytes,
  fetchBytesIO
  ) where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as B
import Data.Maybe

import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Protocol hiding (Message)

fetchBytes::TopicName->Offset->Kafka [B.ByteString]
fetchBytes topic offset = do
  result <- fetch (Offset $ fromIntegral offset) 0 topic
  return $ map tamPayload $ fetchMessages result

fetchBytesIO::TopicName->Offset->IO (Maybe [B.ByteString])
fetchBytesIO topic offset = do
  ret <-
    runKafka (mkKafkaState "block" ("127.0.0.1", 9092)) $ do
      lastOffset <- getLastOffset LatestTime 0 topic

      if (offset > lastOffset)
        then return Nothing
        else return $ Just ()
  
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      fmap (map tamPayload . fetchMessages) $ fetch offset 0 topic


  case ret of
   Left e -> error $ show e
   Right v -> return (Just v)
                                                                                                                                                                                                                      



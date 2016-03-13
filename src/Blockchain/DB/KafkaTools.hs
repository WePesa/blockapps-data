
module Blockchain.DB.KafkaTools (
  fetchBytes
  ) where

import qualified Data.ByteString as B
import Data.Maybe

import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Protocol hiding (Message)

fetchBytes::TopicName->Offset->Kafka [B.ByteString]
fetchBytes topic offset = do
  result <- fetch (Offset $ fromIntegral offset) 0 topic
  return $ map tamPayload $ fetchMessages result




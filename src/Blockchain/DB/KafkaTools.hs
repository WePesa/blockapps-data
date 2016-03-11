
module Blockchain.DB.KafkaTools (
  fetchBytes
  ) where

import qualified Data.ByteString as B
import Data.Maybe

import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Protocol hiding (Message)


fourth4::(a,b,c,d)->d
fourth4 (_, _, _, x) = x

fifth5::(a,b,c,d,e)->e
fifth5 (_, _, _, _, x) = x

fetchBytes::TopicName->Offset->Kafka [B.ByteString]
fetchBytes topic offset = do
  result <- fetch (Offset $ fromIntegral offset) 0 topic
  return $ concat $ map (map (_kafkaByteString . fromJust . _valueBytes . fifth5 . _messageFields .  _setMessage)) $ map _messageSetMembers $ map fourth4 $ head $ map snd $ _fetchResponseFields result




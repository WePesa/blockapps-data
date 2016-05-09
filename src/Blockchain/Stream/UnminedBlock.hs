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


module Blockchain.Stream.UnminedBlock (
  produceUnminedBlocks,
  fetchUnminedBlocks
) where 

import Network.Kafka
import Network.Kafka.Producer
import Network.Kafka.Protocol hiding (Key)

import Blockchain.Stream.Raw
import Blockchain.Data.BlockDB
import Blockchain.Data.RLP

import Control.Monad.State
import Blockchain.KafkaTopics

produceUnminedBlocks::MonadIO m=>[Block]->m ()
produceUnminedBlocks blocks = do
  forM_ blocks $ \block -> do
    _ <- liftIO $ runKafka (mkKafkaState "blockapps-data" ("127.0.0.1", 9092)) $ produceMessages [TopicAndMessage (lookupTopic "unminedblock") $ makeMessage $ rlpSerialize $ rlpEncode $ block]
    --liftIO $ print result
    return ()

fetchUnminedBlocks::Offset->Kafka [Block]
fetchUnminedBlocks = fmap (map (rlpDecode . rlpDeserialize)) . fetchBytes (lookupTopic "unminedblock")

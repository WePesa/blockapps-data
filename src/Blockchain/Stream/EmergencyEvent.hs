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


module Blockchain.Stream.EmergencyEvent (
  EmergencyEventPermanent,
  EmergencyEventTemporary,  
  bytesToEmergencyEventPermanent,
  bytesToEmergencyEventTemporary,
  fetchEmergencyEventsPermanent,
  fetchEmergencyEventsTemporary,
  fetchEmergencyEventsPermanentIO,
  fetchEmergencyEventsTemporaryIO,
  fetchEmergencyEventsPermanentOneIO,
  fetchEmergencyEventsTemporaryOneIO,
  fetchLastEmergencyEventsPermanent,
  fetchLastEmergencyEventsTemporary,
  produceEmergencyEventsPermanent,
  produceEmergencyEventsTemporary    
) where 

import Control.Lens


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Network.Kafka
import Network.Kafka.Producer
import Network.Kafka.Protocol hiding (Key)


import Blockchain.Stream.Raw
import Blockchain.KafkaTopics
import Blockchain.EthConf

import Control.Monad.State

type EmergencyEventPermanent = String
type EmergencyEventTemporary = String
  
{- needed to pass the topic everywhere to test this, which is a bit annoying -}

bytesToEmergencyEventPermanent::B.ByteString->EmergencyEventPermanent
bytesToEmergencyEventPermanent = C.unpack

bytesToEmergencyEventTemporary::B.ByteString->EmergencyEventTemporary
bytesToEmergencyEventTemporary = C.unpack

emergencyEventPermanentToBytes::EmergencyEventPermanent->B.ByteString
emergencyEventPermanentToBytes = C.pack

emergencyEventTemporaryToBytes::EmergencyEventTemporary->B.ByteString
emergencyEventTemporaryToBytes = C.pack

produceEmergencyEventsPermanent::(MonadIO m)=>TopicLabel->[EmergencyEventPermanent]->m Offset
produceEmergencyEventsPermanent topic eEvents = do
  result <- liftIO $ runKafkaConfigured "blockapps-data" $
            produceMessages $ map (TopicAndMessage (lookupTopic topic) . makeMessage . emergencyEventPermanentToBytes) eEvents

  case result of
   Left e -> error $ show e
   Right x -> do
     let [offset] = concat $ map (map (\(_, _, x') ->x') . concat . map snd . _produceResponseFields) x

     return offset


produceEmergencyEventsTemporary::(MonadIO m)=>TopicLabel->[EmergencyEventTemporary]->m Offset
produceEmergencyEventsTemporary topic eEvents = do
  result <- liftIO $ runKafkaConfigured "blockapps-data" $
            produceMessages $ map (TopicAndMessage (lookupTopic topic) . makeMessage . emergencyEventTemporaryToBytes) eEvents

  case result of
   Left e -> error $ show e
   Right x -> do
     let [offset] = concat $ map (map (\(_, _, x') ->x') . concat . map snd . _produceResponseFields) x

     return offset

fetchEmergencyEventsPermanent::TopicLabel->Offset->Kafka [EmergencyEventPermanent]
fetchEmergencyEventsPermanent topic offset = fmap (map bytesToEmergencyEventPermanent) $ fetchBytes (lookupTopic topic) offset 

fetchEmergencyEventsPermanentIO::TopicLabel->Offset->IO (Maybe [EmergencyEventPermanent])
fetchEmergencyEventsPermanentIO topic offset = do
  fmap (fmap (map bytesToEmergencyEventPermanent)) $ fetchBytesIO (lookupTopic topic) offset

fetchEmergencyEventsPermanentOneIO::TopicLabel->Offset->IO (Maybe EmergencyEventPermanent)
fetchEmergencyEventsPermanentOneIO topic offset = do
  fmap (fmap bytesToEmergencyEventPermanent) $ fetchBytesOneIO (lookupTopic topic) offset

{- unsafe, fix -}
fetchLastEmergencyEventsPermanent::TopicLabel->Offset->IO [EmergencyEventPermanent]
fetchLastEmergencyEventsPermanent topic n = do
  ret <-
    runKafkaConfigured "blockapps-data" $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 (lookupTopic topic)
      when (lastOffset == 0) $ error "Block stream is empty, you need to run strato-setup to insert the genesis block."
      let offset = max (lastOffset - n) 0
      fetchEmergencyEventsPermanent topic offset

  case ret of
    Left e -> error $ show e
    Right v -> return v

fetchEmergencyEventsTemporary::TopicLabel->Offset->Kafka [EmergencyEventTemporary]
fetchEmergencyEventsTemporary topic = fmap (map bytesToEmergencyEventTemporary) . fetchBytes (lookupTopic topic)

fetchEmergencyEventsTemporaryIO::TopicLabel->Offset->IO (Maybe [EmergencyEventTemporary])
fetchEmergencyEventsTemporaryIO topic offset = do
  fmap (fmap (map bytesToEmergencyEventTemporary)) $ fetchBytesIO (lookupTopic topic) offset

fetchEmergencyEventsTemporaryOneIO::TopicLabel->Offset->IO (Maybe EmergencyEventTemporary)
fetchEmergencyEventsTemporaryOneIO topic offset = do
  fmap (fmap bytesToEmergencyEventTemporary) $ fetchBytesOneIO (lookupTopic topic) offset

fetchLastEmergencyEventsTemporary::TopicLabel->Offset->IO [EmergencyEventTemporary]
fetchLastEmergencyEventsTemporary topic n = do
  ret <-
    runKafkaConfigured "blockapps-data" $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 (lookupTopic topic)
      when (lastOffset == 0) $ error "Block stream is empty, you need to run strato-setup to insert the genesis block."
      let offset = max (lastOffset - n) 0
      fetchEmergencyEventsTemporary topic offset

  case ret of
    Left e -> error $ show e
    Right v -> return v



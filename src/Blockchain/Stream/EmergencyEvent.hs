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
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module Blockchain.Stream.EmergencyEvent (
  EmergencyEvent(..),
  EmergencyEventPermanent(..),
  EmergencyEventRecoverable(..),
  fetchEmergencyEvents,
  fetchEmergencyEventsIO,
  fetchEmergencyEventsOneIO,
  fetchLastEmergencyEvents,
  produceEmergencyEvents,
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

class EmergencyEvent a where
    toBytes :: a -> B.ByteString
    fromBytes :: B.ByteString -> a
    recoverable :: a -> Bool    

data EmergencyEventPermanent = 
    EmergencyEventPermanent { 
      permanentEventLabel :: String
    } deriving (Show,Read,Eq)

data EmergencyEventRecoverable = 
    EmergencyEventRecoverable {
      recoverableEventLabel :: String   
    } deriving (Show,Read,Eq)

instance EmergencyEvent EmergencyEventPermanent where
    toBytes = C.pack . permanentEventLabel
    fromBytes = EmergencyEventPermanent . C.unpack
    recoverable _ = False 

instance EmergencyEvent EmergencyEventRecoverable where
    toBytes = C.pack . recoverableEventLabel
    fromBytes = EmergencyEventRecoverable . C.unpack
    recoverable _ = True
 
{- needed to pass the topic everywhere to test this, which is a bit annoying -}

produceEmergencyEvents::(MonadIO m, EmergencyEvent e)=>TopicLabel->[e]->m Offset
produceEmergencyEvents topic eEvents = do
  result <- liftIO $ runKafkaConfigured "blockapps-data" $
            produceMessages $ map (TopicAndMessage (lookupTopic topic) . makeMessage . toBytes) eEvents

  case result of
   Left e -> error $ show e
   Right x -> do
     let [offset] = concat $ map (map (\(_, _, x') ->x') . concat . map snd . _produceResponseFields) x

     return offset


fetchEmergencyEvents::(EmergencyEvent e)=>TopicLabel->Offset->Kafka [e]
fetchEmergencyEvents topic offset = fmap (map fromBytes) $ fetchBytes (lookupTopic topic) offset 

fetchEmergencyEventsIO::(EmergencyEvent e)=>TopicLabel->Offset->IO (Maybe [e])
fetchEmergencyEventsIO topic offset = do
  fmap (fmap (map fromBytes)) $ fetchBytesIO (lookupTopic topic) offset

fetchEmergencyEventsOneIO::(EmergencyEvent e)=>TopicLabel->Offset->IO (Maybe e)
fetchEmergencyEventsOneIO topic offset = do
  fmap (fmap fromBytes) $ fetchBytesOneIO (lookupTopic topic) offset

{- unsafe, fix -}
fetchLastEmergencyEvents::(EmergencyEvent e)=>TopicLabel->Offset->IO [e]
fetchLastEmergencyEvents topic n = do
  ret <-
    runKafkaConfigured "blockapps-data" $ do
      stateRequiredAcks .= -1
      stateWaitSize .= 1
      stateWaitTime .= 100000
      lastOffset <- getLastOffset LatestTime 0 (lookupTopic topic)
      when (lastOffset == 0) $ error "Block stream is empty, you need to run strato-setup to insert the genesis block."
      let offset = max (lastOffset - n) 0
      fetchEmergencyEvents topic offset

  case ret of
    Left e -> error $ show e
    Right v -> return v



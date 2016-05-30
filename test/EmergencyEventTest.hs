{-# LANGUAGE ScopedTypeVariables #-}

module EmergencyEventTest where

import Blockchain.Stream.EmergencyEvent

import Test.Tasty.HUnit
import Test.Tasty

topic1= "emergencyEventPermanentTest"

{- assume topics are freshly created via strato-setup in blockapps-data dir -}

emergencyEventUnits = testGroup "Emergency Event Units" [testProduceConsume]

testProduceConsume = testCase "consume . produce = id" $ do
  offset <- produceEmergencyEvents topic1 [EmergencyEventPermanent "Baaaad news!"]
  (event :: Maybe EmergencyEventPermanent) <- fetchEmergencyEventsOneIO topic1 offset

  offset4 <- produceEmergencyEvents topic1 [ EmergencyEventPermanent "Event two", 
                                             EmergencyEventPermanent "event three!", 
                                             EmergencyEventPermanent "event four"]
  event4  <- fetchEmergencyEventsOneIO topic1 (offset4 + 2) -- this + 2 is still weird to me

  assertEqual "fourth event is equal" (Just (EmergencyEventPermanent "event four")) event4
  

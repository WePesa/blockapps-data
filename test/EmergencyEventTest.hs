

module EmergencyEventTest where

import Blockchain.Stream.EmergencyEvent

import Test.Tasty.HUnit
import Test.Tasty

topic1= "emergencyEventPermanentTest"

{- assume topics are freshly created via strato-setup in blockapps-data dir -}

emergencyEventUnits = testGroup "Emergency Event Units" [testProduceConsume]

testProduceConsume = testCase "consume . produce = id" $ do
  offset <- produceEmergencyEventsPermanent topic1 ["Baaaad news!"]
  event <- fetchEmergencyEventsPermanentOneIO topic1 offset

  offset4 <- produceEmergencyEventsPermanent topic1 ["Event two", "event three!", "event four"]
  event4  <- fetchEmergencyEventsPermanentOneIO topic1 (offset4 + 2) -- this + 2 is still weird to me

  assertEqual "fourth event is equal" (Just "event four") event4
  

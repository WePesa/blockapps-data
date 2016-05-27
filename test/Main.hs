
import Test.Tasty
import Test.Tasty.HUnit

import EmergencyEventTest


main = defaultMain tests

tests = testGroup "tests" [unitTests]

unitTests = testGroup "Unit Tests" [emergencyEventUnits]

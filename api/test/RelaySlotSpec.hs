module RelaySlotSpec (spec) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Relay.Slot (utcTimeToSlot)
import Test.Hspec

spec :: Spec
spec = describe "Relay.Slot" $ do
  describe "utcTimeToSlot" $ do
    it "returns Nothing for unknown network" $ do
      let t = posixSecondsToUTCTime 1700000000
      utcTimeToSlot "SomeOtherNet" t `shouldBe` Nothing

    it "converts Preview genesis time to slot 0" $ do
      -- Preview genesis: 2022-11-01T00:00:00Z = posix 1667260800
      let t = posixSecondsToUTCTime 1667260800
      utcTimeToSlot "Preview" t `shouldBe` Just 0

    it "converts Preview time 100 seconds after genesis to slot 100" $ do
      let t = posixSecondsToUTCTime (1667260800 + 100)
      utcTimeToSlot "Preview" t `shouldBe` Just 100

    it "converts Preprod genesis time to shelley start slot" $ do
      -- Preprod shelley start: posix 1654041600, slot 86400
      let t = posixSecondsToUTCTime 1654041600
      utcTimeToSlot "Preprod" t `shouldBe` Just 86400

    it "converts Preprod time 1000 seconds after shelley to slot 87400" $ do
      let t = posixSecondsToUTCTime (1654041600 + 1000)
      utcTimeToSlot "Preprod" t `shouldBe` Just 87400

    it "converts Mainnet shelley start to slot 4492800" $ do
      -- Mainnet shelley: posix 1596059091, slot 4492800
      let t = posixSecondsToUTCTime 1596059091
      utcTimeToSlot "Mainnet" t `shouldBe` Just 4492800

    it "produces increasing slots for increasing times" $ do
      let t1 = posixSecondsToUTCTime 1700000000
          t2 = posixSecondsToUTCTime 1700001000
      case (utcTimeToSlot "Preview" t1, utcTimeToSlot "Preview" t2) of
        (Just s1, Just s2) -> s2 - s1 `shouldBe` 1000
        _ -> expectationFailure "Expected Just for Preview"

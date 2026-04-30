module ApiSpec (spec) where

import Api (hopTimeoutSlot)
import Test.Hspec

spec :: Spec
spec = describe "Api" $ do
  describe "hopTimeoutSlot" $ do
    -- The downstream-most hop (largest hopIndex) should be the *first*
    -- to time out, because it's the receiver-side leg and its claim
    -- gates the whole cascade. Every upstream hop must time out later,
    -- so a bridge that sees the preimage downstream still has time to
    -- claim its upstream lock before that one expires.
    it "is strictly monotone-decreasing in hop index" $ do
      let base = 100_000_000
          n = 4
          ts = [hopTimeoutSlot base n i | i <- [0 .. n - 1]]
      ts `shouldSatisfy` strictlyDecreasing

    it "anchors the downstream-most hop at baseSlot" $ do
      let base = 100_000_000
          n = 3
      hopTimeoutSlot base n (n - 1) `shouldBe` base

    it "spaces hops by exactly hopTimeoutMarginSlots" $ do
      -- We don't import the constant; instead, observe the gap is the
      -- same between every adjacent pair and is > 0.
      let base = 100_000_000
          n = 5
          gaps =
            zipWith
              (-)
              [hopTimeoutSlot base n i | i <- [0 .. n - 2]]
              [hopTimeoutSlot base n i | i <- [1 .. n - 1]]
      gaps `shouldSatisfy` allEqual
      head gaps `shouldSatisfy` (> 0)

    it "single-hop route puts the only hop at baseSlot" $ do
      hopTimeoutSlot 12345 1 0 `shouldBe` 12345
 where
  strictlyDecreasing xs = and (zipWith (>) xs (drop 1 xs))
  allEqual xs = case xs of
    [] -> True
    (h : t) -> all (== h) t

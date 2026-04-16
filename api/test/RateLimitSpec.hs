module RateLimitSpec (spec) where

import Middleware.RateLimit
import Test.Hspec

spec :: Spec
spec = describe "Middleware.RateLimit" $ do
  describe "checkRateLimit" $ do
    it "allows requests under the limit" $ do
      limiter <- newRateLimiter 5
      results <- mapM (\_ -> checkRateLimit limiter "127.0.0.1") [1 :: Int .. 5]
      results `shouldBe` replicate 5 True

    it "blocks requests over the limit" $ do
      limiter <- newRateLimiter 3
      -- Use up the limit
      _ <- mapM (\_ -> checkRateLimit limiter "127.0.0.1") [1 :: Int .. 3]
      -- Next request should be blocked
      result <- checkRateLimit limiter "127.0.0.1"
      result `shouldBe` False

    it "tracks clients independently" $ do
      limiter <- newRateLimiter 2
      -- Fill up client A
      _ <- mapM (\_ -> checkRateLimit limiter "client-a") [1 :: Int .. 2]
      blockedA <- checkRateLimit limiter "client-a"
      blockedA `shouldBe` False
      -- Client B should still be allowed
      allowedB <- checkRateLimit limiter "client-b"
      allowedB `shouldBe` True

    it "allows exactly the limit number of requests" $ do
      limiter <- newRateLimiter 10
      results <- mapM (\_ -> checkRateLimit limiter "test-client") [1 :: Int .. 10]
      all id results `shouldBe` True
      -- 11th should fail
      result <- checkRateLimit limiter "test-client"
      result `shouldBe` False

  describe "cleanupRateLimiter" $ do
    it "does not crash on empty limiter" $ do
      limiter <- newRateLimiter 100
      cleanupRateLimiter limiter
      -- Should still work after cleanup
      result <- checkRateLimit limiter "test"
      result `shouldBe` True

  describe "getClientIp" $ do
    it "extracts IP from request" $ do
      -- getClientIp is tested indirectly through the middleware
      -- Direct testing would require constructing a Request value
      pending

module Specs.Util.SentrySpec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Time
import System.Mem (performMajorGC)
import System.Mem.Weak (deRefWeak, mkWeakPtr)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Model.Sentry.SentryEvent
import Shared.Util.Sentry

sentrySpec =
  describe "Sentry" $
    describe "addBreadcrumb" $ do
      it "keeps only the last breadcrumbs" $ do
        buffer <- newIORef []
        forM_ [1 .. 60] $ \index -> addBreadcrumb buffer =<< breadcrumb (show index)
        breadcrumbs <- readIORef buffer
        fmap (.category) breadcrumbs `shouldBe` fmap show [60, 59 .. 11 :: Int]
      it "releases the breadcrumbs it drops" $ do
        buffer <- newIORef []
        now <- getCurrentTime
        category <- evaluate (show now)
        weakCategory <- mkWeakPtr category Nothing
        addBreadcrumb buffer =<< breadcrumb category
        forM_ [1 .. 60 :: Int] $ \index -> addBreadcrumb buffer =<< breadcrumb (show index)
        performMajorGC
        dropped <- deRefWeak weakCategory
        breadcrumbs <- readIORef buffer
        length breadcrumbs `shouldBe` 50
        isNothing dropped `shouldBe` True

breadcrumb :: String -> IO SentryBreadcrumb
breadcrumb category = do
  now <- getCurrentTime
  return SentryBreadcrumb {timestamp = now, level = "info", category = category, message = "message"}

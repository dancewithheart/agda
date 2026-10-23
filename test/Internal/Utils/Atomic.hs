module Internal.Utils.Atomic ( tests ) where

import Control.Concurrent
import Control.Exception qualified as E
import Control.Monad (replicateM, replicateM_)
import Control.Monad.Except (runExceptT, throwError)
import System.Timeout (timeout)

import Agda.Utils.Atomic

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- A failed update must leave the variable usable, not merely propagate
-- the exception. A timeout turns an abandoned lock into a test failure.
assertContents :: Atomic Int -> Int -> Assertion
assertContents var expected = do
  actual <- timeout 1000000 (readAtomic var)
  actual @?= Just expected

assertFailureRestores :: (Int -> IO (Int, ())) -> Assertion
assertFailureRestores update = do
  var <- newAtomic (42 :: Int)
  result <- E.try @E.ErrorCall (modifyAtomic var update)
  case result of
    Left _   -> pure ()
    Right () -> assertFailure "The update should have thrown an exception"
  assertContents var 42

tests :: TestTree
tests = testGroup "Internal.Utils.Atomic"
  [ testProperty "successful updates publish the new state and return the result" $
      \(old :: Int) (new :: Int) (ret :: Int) -> ioProperty do
        var <- newAtomic old
        result <- modifyAtomic var \_ -> pure (new, ret)
        contents <- readAtomic var
        pure (contents == new && result == ret)
  , testCase "callback exception restores the previous state" $
      assertFailureRestores \_ -> E.throwIO (E.ErrorCall "callback")
  , testCase "undefined result pair restores the previous state" $
      assertFailureRestores \_ -> pure (error "pair")
  , testCase "undefined new state restores the previous state" $
      assertFailureRestores \_ -> pure (error "new state", ())
  , testCase "monadic failure restores the previous state" do
      var <- newAtomic (42 :: Int)
      result <- runExceptT $ modifyAtomic var \_ -> throwError ("abort" :: String)
      result @?= (Left "abort" :: Either String ())
      assertContents var 42
  , testCase "the return value remains lazy" do
      var <- newAtomic (42 :: Int)
      _ <- modifyAtomic var \_ -> pure (43, error "unused return value" :: ())
      assertContents var 43
  , testCase "cancelling an update restores the previous state" do
      var <- newAtomic (42 :: Int)
      entered <- newEmptyMVar
      block <- newEmptyMVar :: IO (MVar ())
      finished <- newEmptyMVar
      tid <- forkIO do
        result <- E.try @E.AsyncException $ modifyAtomic var \_ -> do
          putMVar entered ()
          takeMVar block
          pure (43, ())
        putMVar finished result
      takeMVar entered
      killThread tid
      result <- timeout 5000000 (takeMVar finished)
      case result of
        Just (Left E.ThreadKilled) -> pure ()
        _ -> assertFailure "The worker should have received ThreadKilled"
      assertContents var 42
  , testCase "concurrent updates do not lose increments" do
      var <- newAtomic (0 :: Int)
      finished <- replicateM 8 newEmptyMVar
      mapM_ (\done -> forkIO do
        replicateM_ 100 $ modifyAtomic var \n -> pure (n + 1, ())
        putMVar done ()) finished
      -- Other internal tests can occupy the runtime for a while.
      result <- timeout 5000000 (mapM_ takeMVar finished)
      result @?= Just ()
      assertContents var 800
  ]

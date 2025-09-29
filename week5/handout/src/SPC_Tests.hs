module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
  ( Job (Job),
    JobDoneReason (..),
    JobStatus (..),
    jobAdd,
    jobCancel,
    jobStatus,
    jobWait,
    pingSPC,
    startSPC,
  )
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 0
          y <- pingSPC spc
          y @?= 1
          z <- pingSPC spc
          z @?= 2,
        testCase "adding job" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job (pure ()) 1
          pure (),
        testCase "adding job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          r <- jobStatus spc j
          r @?= Just JobRunning,
        testCase "canceling job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          jobCancel spc j
          r <- jobStatus spc j
          r @?= Just (JobDone DoneCancelled),
        testCase "running job" $ do
          ref <- newIORef False
          spc <- startSPC
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r <- jobWait spc j
          r @?= Just Done
          x <- readIORef ref
          x @?= True
      ]

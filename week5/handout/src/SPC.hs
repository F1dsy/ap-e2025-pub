{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
    Job(..)
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg -- TODO: add messages.
  = MsgPing (ReplyChan Int)
  | MsgJobAdd (JobId, Job)

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState 
  { spcPingCounter :: Int,
    spcJobs :: [(JobId, Job)]
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \s -> pure (x,s)
  (<*>) :: SPCM (a -> b) -> SPCM a -> SPCM b
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'
 
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

put :: SPCState -> SPCM ()
put state = SPCM $  \_ -> pure ((), state)
    
io :: IO a -> SPCM a
io ioa = SPCM $ \state -> do
  a <- ioa
  pure (a, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM m) = 
  m state >>= \(a, _) -> pure a
  

startSPC :: IO SPC
startSPC = do
  let initial_state = SPCState {spcPingCounter = 0, spcJobs = []}
  server <-  spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC server
  

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  r <- io $ receive c
  case r of
        (MsgPing r') -> do 
          state <- get 
          put $ state {spcPingCounter = succ $ spcPingCounter state}
          io $ reply r' $ spcPingCounter state
        (MsgJobAdd (i, j)) -> do undefined

  

pingSPC :: SPC -> IO Int
pingSPC (SPC spc) = do
  requestReply spc MsgPing

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC spc) (job) = undefined
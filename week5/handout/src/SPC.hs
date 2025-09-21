module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
    Job (..),
    jobAdd,
    jobStatus,
    jobCancel,
    JobDoneReason (..),
    JobStatus (..),
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

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcPingCounter :: Int,
    spcJobCounter :: JobId,
    spcJobsPending :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)]
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM (\s -> pure (x, s))
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  a <- m
  pure (a, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM m) = fst <$> m state

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC s) jobId = requestReply s (MsgJobStatus jobId)

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC s) jobId = requestReply s (MsgJobCancel jobId)

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC s) j = requestReply s (MsgJobAdd j)

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg -- TODO: add messages.
  = MsgPing (ReplyChan Int)
  | MsgJobAdd Job (ReplyChan JobId)
  | MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | MsgJobCancel JobId

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

startSPC :: IO SPC
startSPC = do
  let initialState = SPCState {spcPingCounter = 0, spcJobCounter = JobId 0, spcJobsPending = [], spcJobsDone = []}
  server <- spawn (runSPCM initialState . forever . handleMsg)
  pure $ SPC server

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    (MsgPing rsvp) -> do
      state <- get
      io $ reply rsvp $ spcPingCounter state
      put
        SPCState
          { spcPingCounter = succ $ spcPingCounter state,
            spcJobCounter = spcJobCounter state,
            spcJobsPending = spcJobsPending state,
            spcJobsDone = spcJobsDone state
          }
    (MsgJobAdd j rsvp) -> do
      state <- get
      let jobId = spcPingCounter state
      put
        SPCState
          { spcPingCounter = spcPingCounter state,
            spcJobCounter = JobId $ succ jobId,
            spcJobsPending = (JobId jobId, j) : spcJobsPending state,
            spcJobsDone = spcJobsDone state
          }
      io $ reply rsvp $ JobId jobId
    (MsgJobStatus jobId rsvp) -> do
      state <- get

      io $ reply rsvp $ case lookup jobId $ spcJobsPending state of
        Just _ -> Just JobPending
        _ -> Nothing
    (MsgJobCancel jobId) -> do
      state <- get
      case lookup jobId $ spcJobsPending state of
        Just job -> undefined
        _ -> Nothing
      put
        SPCState
          { spcPingCounter = spcPingCounter state,
            spcJobCounter = spcJobCounter state,
            spcJobsPending = spcJobsPending state,
            spcJobsDone = (jobId, DoneCancelled) : spcJobsDone state
          }

pingSPC :: SPC -> IO Int
pingSPC (SPC s) = requestReply s MsgPing

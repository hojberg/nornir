{-# LANGUAGE TemplateHaskell #-}

module Nornir.Task where

import qualified Data.Text                     as T
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Lens.Micro.Platform
import           Data.List                     as List
-- import           Date.Time

data TaskStatus
  = Incomplete
  | Complete
  | Logged
  deriving (Show, Eq)

data Due
  = Undecided
  | Unscheduled
  | Today
  | Scheduled -- Date
  | Someday
  deriving (Show, Eq)

data Task = Task
  { _id :: UUID
  , _name :: T.Text
  , _description :: T.Text
  , _status :: TaskStatus
  , _projectId :: Maybe UUID
  , _due :: Due
  } deriving (Show, Eq)


makeLenses ''Task


new :: Due -> T.Text -> UUID -> Task
new due name id = Task
  { _id          = id
  , _name        = name
  , _description = T.pack ""
  , _status      = Incomplete
  , _projectId   = Nothing
  , _due         = due
  }


build :: Due -> T.Text -> IO Task
build due name = fmap (new due name) nextRandom


invertTaskStatus :: TaskStatus -> TaskStatus
invertTaskStatus Complete   = Incomplete
invertTaskStatus Incomplete = Complete


toggleCompletion :: UUID -> Task -> Task
toggleCompletion taskId task = if taskId == (_id task)
  then
    let newStatus = invertTaskStatus $ _status task
    in  task { _status = newStatus }
  else task


logCompleted :: Task -> Task
logCompleted task = case _status task of
  Incomplete -> task
  Complete   -> task { _status = Logged }
  Logged     -> task


indexOfTaskId :: Maybe UUID -> [Task] -> Maybe Int
indexOfTaskId _       [] = Nothing
indexOfTaskId Nothing _  = Nothing
indexOfTaskId (Just tid) tasks =
  List.find (\t -> _id t == tid) tasks >>= (\t -> elemIndex t tasks)

{-# LANGUAGE OverloadedStrings #-}

module Task where


import qualified Data.Text                     as T
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Data.List                     as List
import Data.Time


data TaskStatus
  = Incomplete
  | Complete
  | Logged
  deriving (Show, Read, Eq, Bounded, Enum)


data Due
  = Undecided
  | Unscheduled
  | Today
  | Scheduled Day
  | Someday
  deriving (Show, Read, Eq)


data Task = Task
  { tId :: UUID
  , name :: T.Text
  , description :: T.Text
  , status :: TaskStatus
  , due :: Due
  } deriving (Show, Eq)


make :: Due -> T.Text -> UUID -> Task
make due name id = Task
  { tId         = id
  , name        = name
  , description = T.pack ""
  , status      = Incomplete
  , due         = due
  }


build :: Due -> T.Text -> IO Task
build due name = fmap (make due name) nextRandom


invertTaskStatus :: TaskStatus -> TaskStatus
invertTaskStatus Complete   = Incomplete
invertTaskStatus Incomplete = Complete


toggleCompletion :: UUID -> Task -> Task
toggleCompletion taskId task = if taskId == (tId task)
  then
    let newStatus = invertTaskStatus $ status task
    in  task { status = newStatus }
  else task


logCompleted :: Task -> Task
logCompleted task = case status task of
  Incomplete -> task
  Complete   -> task { status = Logged }
  Logged     -> task


indexOfTaskId :: Maybe UUID -> [Task] -> Maybe Int
indexOfTaskId _       [] = Nothing
indexOfTaskId Nothing _  = Nothing
indexOfTaskId (Just id) tasks =
  List.find (\t -> tId t == id) tasks >>= (\t -> elemIndex t tasks)

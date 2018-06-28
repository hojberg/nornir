{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Nornir.Types where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Lens.Micro.Platform

data Project = Project
  { p_id :: UUID
  , p_name :: T.Text
  , p_description :: T.Text
  } deriving (Show, Eq)

data TaskStatus
  = Incomplete
  | Complete
  deriving (Show, Eq)

data Task = Task
  { _id :: UUID
  , _name :: T.Text
  , _description :: T.Text
  , _status :: TaskStatus
  , _projectId :: Maybe UUID
  } deriving (Show, Eq)

makeLenses ''Task

newTask :: T.Text -> UUID -> Task
newTask name id = Task
  { _id          = id
  , _name        = name
  , _description = T.pack ""
  , _status      = Incomplete
  , _projectId   = Nothing
  }

buildTask :: T.Text -> IO Task
buildTask name = fmap (newTask name) nextRandom

invertTaskStatus :: TaskStatus -> TaskStatus
invertTaskStatus Complete   = Incomplete
invertTaskStatus Incomplete = Complete

toggleCompletionOfTask :: UUID -> Task -> Task
toggleCompletionOfTask taskId task = if taskId == (_id task)
  then
    let newStatus = invertTaskStatus $ _status task
    in  task { _status = newStatus }
  else task

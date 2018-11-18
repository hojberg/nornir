{-# LANGUAGE OverloadedStrings #-}

module Task where

import qualified Data.Text                     as T
import           Data.UUID                      ( UUID
                                                , fromText
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Data.List                     as List
import           Data.Time
import           Text.Read
import           Data.String
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok


-- CORE

instance ToField UUID where
  toField = SQLText . T.pack . show

instance FromField UUID where
  fromField f =
    case f of
      (Field (SQLText rawUuid) _) ->
        let
          res = fromText rawUuid
        in
        case res of
          Nothing -> returnError ConversionFailed f ""
          Just uuid -> Ok uuid
      _ -> returnError ConversionFailed f ""

data TaskStatus
  = Incomplete
  | Complete
  | Logged
  deriving (Show, Read, Eq, Bounded, Enum)

instance ToField TaskStatus where
  toField = SQLText . T.pack . show

instance FromField TaskStatus where
  fromField (Field (SQLText "Incomplete") _) = Ok Incomplete
  fromField (Field (SQLText "Complete") _) = Ok Complete
  fromField (Field (SQLText "Logged") _) = Ok Logged
  fromField f = returnError ConversionFailed f "need 'Incomplete', 'Complete', or 'Logged'"


data Due
  = Undecided
  | Unscheduled
  | Today
  | Scheduled Day
  | Someday
  deriving (Show, Read, Eq)

instance ToField Due where
  toField = SQLText . T.pack . show

instance FromField Due where
  fromField (Field (SQLText "Undecided") _) = Ok Undecided
  fromField (Field (SQLText "Unscheduled") _) = Ok Unscheduled
  fromField (Field (SQLText "Today") _) = Ok Today
  fromField (Field (SQLText "Scheduled") _) = Ok Today -- TODO
  fromField (Field (SQLText "Someday") _) = Ok Someday
  fromField f = returnError ConversionFailed f "need 'Undecided', 'Unscheduled', 'Today', 'Scheduled', 'or 'Someday'"


data Task = Task
  { tId :: UUID
  , name :: T.Text
  , description :: T.Text
  , status :: TaskStatus
  , due :: Due
  } deriving (Show, Eq)

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field

instance ToRow Task where
  toRow (Task tId name description status due) = toRow (tId, name, description, status, due)


-- Helper functions

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
toggleCompletion taskId task = if taskId == tId task
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
  let finder t = tId t == id
  in  List.find finder tasks >>= (\t -> elemIndex t tasks)

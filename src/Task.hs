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
import           Data.Time.Calendar
import           Data.Ord                       ( comparing )


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
  | Started
  | Complete
  deriving (Show, Read, Eq, Bounded, Enum)


instance ToField TaskStatus where
  toField = SQLText . T.pack . show


instance FromField TaskStatus where
  fromField (Field (SQLText "Incomplete") _) = Ok Incomplete
  fromField (Field (SQLText "Complete") _) = Ok Complete
  fromField (Field (SQLText "Started") _) = Ok Started
  fromField f = returnError ConversionFailed f "need 'Incomplete', 'Complete', or 'Started"


data Due
  = Next
  | OnDate Day
  deriving (Show, Read, Eq)

instance ToField Due where
  toField = SQLText . T.pack . show

instance FromField Due where
  fromField field =
    case field of
      (Field (SQLText text) _)
        | text == "Next" -> Ok Next
        | "OnDate" `isPrefixOf` T.unpack text ->
          let
            dayString = T.unpack $ T.replace "OnDate " ""  text
            dayM = parseTimeM True defaultTimeLocale "%Y-%m-%d" dayString :: Maybe Day

            res = case dayM of
              Nothing -> returnError ConversionFailed field "Could not parse day for OnDate"
              Just day -> Ok (OnDate day)
          in
            res
        | otherwise -> returnError ConversionFailed field "need 'Next or 'OnDate'"


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


progressTaskStatus :: TaskStatus -> TaskStatus
progressTaskStatus Incomplete = Started
progressTaskStatus Started    = Complete
progressTaskStatus Complete   = Incomplete


toggleCompletion :: UUID -> Task -> Task
toggleCompletion taskId task = if taskId == tId task
  then
    let newStatus = progressTaskStatus $ status task
    in  task { status = newStatus }
  else task


indexOfTaskId :: Maybe UUID -> [Task] -> Maybe Int
indexOfTaskId _       [] = Nothing
indexOfTaskId Nothing _  = Nothing
indexOfTaskId (Just id) tasks =
  let finder t = tId t == id in List.find finder tasks >>= (`elemIndex` tasks)


score :: Task -> Double
score task = case status task of
  Incomplete -> 0.0
  Started    -> 0.5
  Complete   -> 1.0


totalScore :: [Task] -> (Double, Int)
totalScore tasks =
  let theScore = List.foldl (+) 0.0 (List.map score tasks)
  in  (theScore, List.length tasks)


-- mostRecentTaskDay :: [Task] -> Maybe Day
-- mostRecentTaskDay tasks =


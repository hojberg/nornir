{-# LANGUAGE OverloadedStrings #-}

module DB where

import           Task
import           Control.Applicative
import           Data.UUID                      ( UUID )
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.ToField
import           Data.Maybe
import           System.Directory
import           System.IO
import           Data.Text                      ( pack )
import qualified DB.Migrations                 as Migrations
import           DB.Utils                       ( toQuery )
import qualified Data.Text                     as T

removeTask :: UUID -> IO ()
removeTask taskId = dbFilePath >>= \path -> withConnection path $ \conn ->
  execute conn
          "DELETE FROM tasks \
  \WHERE id = ?"
          (Only (toField taskId :: SQLData))

updateTask :: Task -> IO ()
updateTask task = dbFilePath >>= \path -> withConnection path $ \conn ->
  execute
    conn
    "UPDATE tasks \
  \SET name = ?, description = ?, status = ?, due = ? \
  \WHERE id = ?"
    ( name task :: T.Text
    , description task :: T.Text
    , (toField $ status task) :: SQLData
    , (toField $ due task) :: SQLData
    , (toField $ tId task) :: SQLData
    )

addTask task = dbFilePath >>= \path -> withConnection path $ \conn -> execute
  conn
  "INSERT INTO tasks \
  \(id, name, description, status, due) \
  \VALUES (?, ?, ?, ?, ?)"
  task


dbFilePath :: IO FilePath
dbFilePath =
  let homeFolder = "/.nornir/nornir.db"
  in  fmap (++ homeFolder) getHomeDirectory


init :: IO [Task]
init = do
  dbFile <- dbFilePath
  conn   <- open dbFile
  Migrations.createSchemaMigrationsTable conn
  Migrations.migrateAll conn
  tasks <-
    query_ conn "SELECT id, name, description, status, due from tasks" :: IO
      [Task]
  close conn
  pure tasks

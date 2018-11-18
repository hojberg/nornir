{-# LANGUAGE OverloadedStrings #-}

module DB.Migrations where
import           Data.Text                      ( pack )
import           Database.SQLite.Simple


type Version = Int

type Migration = (Int, Query)

-- Helpers

migrationsForVersion :: Version -> [Migration]
migrationsForVersion version =
  let byVersion (v, _) = v /= version in filter byVersion allMigrations

toQuery = Query . pack

-- Migrations

allMigrations :: [Migration]
allMigrations = [createTasks]

createTasks :: Migration
createTasks =
  ( 1
  , toQuery
    "CREATE TABLE IF NOT EXISTS tasks ( \
    \ id TEXT, \
    \ name TEXT, \
    \ description TEXT, \
    \ status TEXT, due TEXT \
  \ );"
  )

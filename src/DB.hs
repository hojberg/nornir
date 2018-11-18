{-# LANGUAGE OverloadedStrings #-}

module DB where

import           Task
import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Data.Maybe
import           System.Directory
import           System.IO
import           Data.Text                      ( pack )
import qualified DB.Migrations                 as Migrations


defaultDBFilePath :: String
defaultDBFilePath = "/Users/shojberg/.config/nornir/nornir.db"

newtype SchemaMigration = SchemaMigration { version :: Migrations.Version } deriving (Show, Eq)

instance FromRow SchemaMigration where
  fromRow = SchemaMigration <$> field

instance ToRow SchemaMigration where
  toRow (SchemaMigration version) = toRow version

instance ToRow Int where
  toRow = toRow

createSchemaMigrationsTable :: Connection -> IO ()
createSchemaMigrationsTable conn = execute_
  conn
  "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY)"

queryVersion :: Connection -> IO (Maybe Migrations.Version)
queryVersion conn =
  let migrations =
        query_ conn
               "SELECT * FROM schema_migrations ORDER BY version DESC LIMIT 1" :: IO
            [SchemaMigration]
      toDate migrations = case migrations of
        []       -> Nothing
        (x : xs) -> Just (version x)
  in  fmap toDate migrations


pathToVersion :: FilePath -> String
pathToVersion path = path

getMigrations :: Maybe Migrations.Version -> IO [Migrations.Migration]
getMigrations version =
  let migrations = case version of
        Nothing -> Migrations.allMigrations
        Just v  -> Migrations.migrationsForVersion v
  in  pure migrations

runMigration :: Connection -> Migrations.Migration -> IO ()
runMigration conn (version, sql)
  = let
      addVersionSql = Query
        (pack
          (  "INSERT INTO schema_migrations (version) VALUES ("
          ++ show version
          ++ ")"
          )
        )
    in  do
          execute_ conn sql
          execute_ conn addVersionSql


migrateAll :: Connection -> IO ()
migrateAll conn =
  fmap head (queryVersion conn >>= getMigrations >>= mapM (runMigration conn))


init :: Maybe FilePath -> IO [Task]
init dbFilePath =
  let dbFile = fromMaybe defaultDBFilePath dbFilePath
  in
    do
      conn <- open dbFile
      createSchemaMigrationsTable conn
      migrateAll conn
      tasks <-
        query_ conn "SELECT id, name, description, status, due from tasks" :: IO
          [Task]
      close conn
      pure tasks

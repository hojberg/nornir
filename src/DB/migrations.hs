{-# LANGUAGE OverloadedStrings #-}

module DB.Migrations where
import           Database.SQLite.Simple
import           DB.Utils                       ( toQuery )

type Version = Int
type Migration = (Int, Query)

newtype SchemaMigration = SchemaMigration { version :: Version } deriving (Show, Eq)

instance FromRow SchemaMigration where
  fromRow = SchemaMigration <$> field

instance ToRow SchemaMigration where
  toRow (SchemaMigration version) = toRow version

instance ToRow Int where
  toRow = toRow

-- Helpers

migrationsForVersion :: Version -> [Migration]
migrationsForVersion version =
  let byVersion (v, _) = v /= version in filter byVersion allMigrations

getMigrations :: Maybe Version -> IO [Migration]
getMigrations version =
  let migrations = case version of
        Nothing -> allMigrations
        Just v  -> migrationsForVersion v
  in  pure migrations

runMigration :: Connection -> Migration -> IO ()
runMigration conn (version, sql) =
  let addVersionSql =
        toQuery "INSERT INTO schema_migrations (version) VALUES (?)"
  in  do
        execute_ conn sql
        execute conn addVersionSql (Only (version :: Int))

queryVersion :: Connection -> IO (Maybe Version)
queryVersion conn =
  let migrations =
        query_ conn
               "SELECT * FROM schema_migrations ORDER BY version DESC LIMIT 1" :: IO
            [SchemaMigration]
      toDate migrations = case migrations of
        []       -> Nothing
        (x : xs) -> Just (version x)
  in  fmap toDate migrations

migrateAll :: Connection -> IO ()
migrateAll conn =
  fmap head (queryVersion conn >>= getMigrations >>= mapM (runMigration conn))

createSchemaMigrationsTable :: Connection -> IO ()
createSchemaMigrationsTable conn = execute_
  conn
  "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY)"

allMigrations :: [Migration]
allMigrations = [createTasks]

-- Migrations

createTasks :: Migration
createTasks =
  ( 1542553908
  , toQuery
    "CREATE TABLE IF NOT EXISTS tasks ( \
    \ id TEXT, \
    \ name TEXT, \
    \ description TEXT, \
    \ status TEXT, \
    \ due TEXT \
  \ );"
  )

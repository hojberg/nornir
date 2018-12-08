module DB.Utils where

import           Data.Text                      ( pack )
import           Database.SQLite.Simple

toQuery = Query . pack


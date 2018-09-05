{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Nornir.Types where

import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Lens.Micro.Platform

data Project = Project
  { p_id :: UUID
  , p_name :: T.Text
  , p_description :: T.Text
  } deriving (Show, Eq)


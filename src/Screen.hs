{-# LANGUAGE UnicodeSyntax #-}

module Screen where

data Screen
  = Next
  | Today
  | Yesterday
  | ExtraCredit
  deriving (Show, Ord, Eq)

allScreens :: [Screen]
allScreens = [Next, Today, Yesterday, ExtraCredit]

format :: Screen -> String
format Next        = "✉  Next"
format Today       = "★  Today"
format Yesterday   = " ☾ Yesterday"
format ExtraCredit = "⫹⫺ Extra Credit"

nextScreen :: Screen -> Screen
nextScreen screen = case screen of
  Next        -> Today
  Today       -> Yesterday
  Yesterday   -> ExtraCredit
  ExtraCredit -> ExtraCredit

previousScreen :: Screen -> Screen
previousScreen screen = case screen of
  Next        -> Next
  Today       -> Next
  Yesterday   -> Today
  ExtraCredit -> Yesterday

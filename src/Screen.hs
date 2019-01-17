{-# LANGUAGE UnicodeSyntax #-}

module Screen where

data Screen
  = Next
  | Today
  | Yesterday
  | ExtraCredit
  deriving (Show, Ord, Eq)

allScreens :: [Screen]
allScreens = [Next, Today, Yesterday]

format :: Screen -> String
format Next      = "✉  Next"
format Today     = "★  Today"
format Yesterday = " ☾ Yesterday"

nextScreen :: Screen -> Screen
nextScreen screen = case screen of
  Next      -> Today
  Today     -> Yesterday
  Yesterday -> Yesterday

previousScreen :: Screen -> Screen
previousScreen screen = case screen of
  Next      -> Next
  Today     -> Next
  Yesterday -> Today

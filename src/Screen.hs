{-# LANGUAGE UnicodeSyntax #-}

module Screen where

data Screen
  = Inbox
  | Today
  | Upcoming
  | Anytime
  | Someday
  deriving (Show, Ord, Eq)

allScreens :: [Screen]
allScreens = [Inbox, Today, Upcoming, Anytime, Someday]

format :: Screen -> String
format Inbox    = "✉  Inbox"
format Today    = "★  Today"
format Upcoming = "⚅  Upcoming"
format Anytime  = "⫹⫺ Anytime"
format Someday  = " ☾ Someday"

nextScreen :: Screen -> Screen
nextScreen screen = case screen of
  Inbox    -> Today
  Today    -> Upcoming
  Upcoming -> Anytime
  Anytime  -> Someday
  Someday  -> Someday

previousScreen :: Screen -> Screen
previousScreen screen = case screen of
  Inbox    -> Inbox
  Today    -> Inbox
  Upcoming -> Today
  Anytime  -> Upcoming
  Someday  -> Anytime

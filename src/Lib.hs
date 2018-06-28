{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib where

import           Brick.AttrMap              (AttrMap, attrMap)
import qualified Brick.AttrMap              as A
import           Brick.Main                 (App (..), defaultMain,
                                             neverShowCursor)
import qualified Brick.Main                 as M
import           Brick.Types                (BrickEvent (..), EventM, Next,
                                             Widget)
import           Brick.Util                 (on, fg)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as C
import qualified Brick.Widgets.Edit         as E
import           Control.Monad              (void)
import           Data.List                  as List
import           Data.Text                  (unpack)
import qualified Data.Text                  as T
import           Data.UUID                  (UUID)
import           Graphics.Vty               (Event (EvKey),
                                             Key (KChar, KEnter, KEsc))
import qualified Graphics.Vty               as V
import           Nornir.Types               (Project, Task (..),
                                             TaskStatus (..), buildTask, toggleCompletionOfTask)

-- MODEL


data Screen
  = Inbox
  | Today
  | Upcoming
  | Anytime
  | Someday
  | Log
  | ViewProject UUID


data Name
  = Info
  | TextBox
  deriving (Show, Ord, Eq)


data Mode
  = AwaitingCommand
  | NewTask (E.Editor String Name)
  | EditTask UUID
             (E.Editor String Name)


data Model = Model
  { tasks          :: [Task]
  , projects       :: [Project]
  , mode           :: Mode
  , selectedTaskId :: Maybe UUID
  }


initialState :: IO Model
initialState =
  do
      \t -> Model
        { tasks          = [t]
        , projects       = []
        , mode           = AwaitingCommand
        , selectedTaskId = Nothing
        }
    <$> buildTask "Buy Milk"


-- UPDATE

findIndexOfSelectedTaskId :: Maybe UUID -> [Task] -> Maybe Int
findIndexOfSelectedTaskId _       [] = Nothing
findIndexOfSelectedTaskId Nothing _  = Nothing
findIndexOfSelectedTaskId (Just selectedId) tasks =
  List.find (\t -> _id t == selectedId) tasks >>= (\t -> elemIndex t tasks)

nextIndex :: [a] -> Maybe Int -> Maybe Int
nextIndex []   _        = Nothing
nextIndex _    Nothing  = Just 0
nextIndex list (Just i) = if i + 1 == length list then Just i else Just (i + 1)

prevIndex :: [a] -> Maybe Int -> Maybe Int
prevIndex []   _        = Nothing
prevIndex _    Nothing  = Just 0
prevIndex list (Just i) = if i - 1 < 0 then Just 0 else Just (i - 1)

previousTaskId :: Model -> Maybe UUID
previousTaskId model =
  fmap _id
    $ fmap (\i -> tasks model !! i)
    $ prevIndex (tasks model)
    $ (findIndexOfSelectedTaskId (selectedTaskId model) (tasks model))

selectPreviousTask :: Model -> Model
selectPreviousTask model = model { selectedTaskId = previousTaskId model }

nextTaskId :: Model -> Maybe UUID
nextTaskId model =
  fmap _id
    $ fmap (\i -> tasks model !! i)
    $ nextIndex (tasks model)
    $ (findIndexOfSelectedTaskId (selectedTaskId model) (tasks model))


selectNextTask :: Model -> Model
selectNextTask model = model { selectedTaskId = nextTaskId model }


deleteSelectedTask :: Model -> Model
deleteSelectedTask model = case selectedTaskId model of
  Nothing -> model
  Just selectedId ->
    let newTasks = List.filter (\t -> _id t /= selectedId) $ tasks model
        newSelectedTaskId = nextTaskId model
    in  model { tasks = newTasks, selectedTaskId = newSelectedTaskId }


toggleCompletionOfSelectedTask :: Model -> Model
toggleCompletionOfSelectedTask model = case selectedTaskId model of
  Nothing -> model
  Just selectedId ->
    let newTasks = List.map (toggleCompletionOfTask selectedId) $ tasks model
    in  model { tasks = newTasks }

awaitingCommand :: Model -> BrickEvent Name e -> EventM Name (Next Model)
awaitingCommand model (VtyEvent (EvKey (KChar 'n') [])) =
  M.continue $ model { mode = NewTask (E.editor TextBox Nothing "") }
awaitingCommand model (VtyEvent (EvKey (KChar 'k') [])) =
  M.continue $ selectPreviousTask model
awaitingCommand model (VtyEvent (EvKey (KChar 'j') [])) =
  M.continue $ selectNextTask model
awaitingCommand model (VtyEvent (EvKey (KChar 'd') [])) =
  M.continue $ deleteSelectedTask model
awaitingCommand model (VtyEvent (EvKey (KChar ' ') [])) =
  M.continue $ toggleCompletionOfSelectedTask model
awaitingCommand model (VtyEvent (EvKey (KChar 'q') [])) = M.halt model
awaitingCommand model (VtyEvent (EvKey KEsc        [])) = M.halt model
awaitingCommand model _ = M.continue model


update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update model evt = case mode model of
  NewTask editor -> case evt of
    (VtyEvent (EvKey KEsc [])) -> M.continue $ model { mode = AwaitingCommand }
    (VtyEvent (EvKey KEnter [])) ->
      let
        taskName     = T.pack $ unlines $ E.getEditContents editor
        currentTasks = tasks model
      in
        if taskName == ""
          then M.continue $ model { mode = AwaitingCommand }
          else M.suspendAndResume $ do
            newTask <- buildTask taskName
            return $ model { mode  = AwaitingCommand
                           , tasks = currentTasks ++ [newTask]
                           }
    (VtyEvent ev) -> M.continue =<< fmap
      (\newEd -> model { mode = NewTask newEd })
      (E.handleEditorEvent ev editor)
    _ -> M.continue model
  EditTask id editor -> M.continue model
  AwaitingCommand    -> awaitingCommand model evt


-- VIEW

fill :: Widget Name
fill = C.fill ' '

navItem :: String -> Widget Name
navItem item = C.vBox
  [C.vLimit 1 $ C.hBox [itemName, fill, B.vBorder, count], C.hBox [B.hBorder]]
 where
  itemName = C.str item
  count    = C.str "   3 "

nav :: Widget Name
nav =
  C.withBorderStyle BS.unicodeRounded
    .  B.border
    $  C.hLimit 30
    $  C.vBox
    $  (List.map
         navItem
         [ " ✉  Inbox "
         , " ★  Today "
         , "  ▦ Upcoming "
         , " ⫹⫺ Anytime "
         , "  ☾ Someday "
         ]
       )
    ++ [fill]

unchecked :: String
unchecked = "☐ "

checked :: String
checked = "☑ "

isSelected :: Maybe UUID -> Task -> Bool
isSelected Nothing           _    = False
isSelected (Just selectedId) task = _id task == selectedId

formatTask :: Task -> String
formatTask task = case _status task of
  Incomplete -> unchecked ++ " " ++ (unpack $ _name task)
  Complete   -> checked ++ " " ++ (unpack $ _name task)

taskRow :: Maybe UUID -> Task -> Widget Name
taskRow selectedTaskId task = row
 where
  row = if isSelected selectedTaskId task
    then C.vBox
      [ C.vLimit 1
      $  C.withAttr "selected"
      $  C.str
      $  " "
      ++ formatTask task
      ++ " "
      , C.hBox [B.hBorder]
      ]
    else C.vBox
      [C.vLimit 1 $ C.str $ " " ++ formatTask task ++ " ", C.hBox [B.hBorder]]

content :: Maybe UUID -> [Task] -> Widget Name
content selectedTaskId tasks =
  C.withBorderStyle BS.unicodeRounded
    .  B.border
    $  C.vBox
    $  (List.map (taskRow selectedTaskId) tasks)
    ++ [fill]

calendarDay :: Widget Name
calendarDay =
  C.withBorderStyle BS.unicodeRounded
    . B.border
    $ C.hLimit 50
    $ C.center
    $ C.str " "

workspace :: Model -> Widget Name
workspace model = C.hBox
  [ nav
  , C.str " "
  , content (selectedTaskId model) $ tasks model
  , C.str " "
  , calendarDay
  ]

cmdline :: Model -> Widget Name
cmdline model =
  let result = case mode model of
        NewTask editor -> C.vBox [E.renderEditor (C.str . unlines) True editor]
        EditTask id editor ->
          C.vBox [E.renderEditor (C.str . unlines) True editor]
        AwaitingCommand -> C.vBox [C.str "Awaiting command"]
  in  C.withBorderStyle BS.unicodeRounded
      . B.border
      $ C.vLimit 1
      $ C.center
      $ result

draw :: Model -> [Widget Name]
draw model = [ui]
  where ui = C.joinBorders $ C.vBox [workspace model, cmdline model]

styles :: A.AttrMap
styles = A.attrMap
  V.defAttr
  [ (B.borderAttr     , fg V.black)
  , (E.editFocusedAttr, fg V.green)
  , ("selected"       , fg V.blue)
  ]

-- APP
app :: App Model e Name
app = App
  { appDraw         = draw
  , appHandleEvent  = update
  , appAttrMap      = const styles
  , appStartEvent   = return
  , appChooseCursor = neverShowCursor
  }

run :: IO ()
run = void $ initialState >>= M.defaultMain app

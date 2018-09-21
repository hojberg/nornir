{-# LANGUAGE OverloadedStrings #-}

module Nornir where

import           Brick.AttrMap                  ( AttrMap
                                                , attrMap
                                                )
import qualified Brick.AttrMap                 as A
import           Brick.Main                     ( App(..)
                                                , defaultMain
                                                , neverShowCursor
                                                )
import qualified Brick.Main                    as M
import           Brick.Types                    ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , Widget
                                                )
import           Brick.Util                     ( on
                                                , fg
                                                , bg
                                                )
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Core            as C
import qualified Brick.Widgets.Edit            as E
import           Control.Monad                  ( void )
import           Data.List                     as List
import           Data.Text                      ( unpack )
import qualified Data.Text                     as T
import           Data.UUID                      ( UUID )
import           Graphics.Vty                   ( Event(EvKey)
                                                , Key(KChar, KEnter, KEsc)
                                                )
import qualified Graphics.Vty                  as V

import           Util                    ( nextIndex
                                                , prevIndex
                                                )
import           Task                    ( Task(..)
                                                , TaskStatus(..)
                                                )
import qualified Task
import qualified UI
import Screen (Screen(..))
import qualified Screen

-- MODEL

data Name
  = Info
  | TextBox
  deriving (Show, Ord, Eq)


data Mode
  = AwaitingCommand
  | NewTask (E.Editor String Name)
  | EditTask UUID (E.Editor String Name)


data Model = Model
  { tasks          :: [Task]
  , mode           :: Mode
  , screen         :: Screen
  , selectedTaskId :: Maybe UUID
  }


initialState :: IO Model
initialState =
  do
      \t -> Model
        { tasks          = [t]
        , screen         = Screen.Today
        , mode           = AwaitingCommand
        , selectedTaskId = Nothing
        }
    <$> Task.build Task.Today "Buy Milk"


-- UPDATE

tasksByScreen :: Screen -> [Task] -> [Task]
tasksByScreen screen tasks =
  let
    taskDue = screenToTaskDue screen

    forScreen t =
      due t == taskDue && status t /= Task.Logged
  in
    List.filter forScreen tasks


tasksForCurrentScreen :: Model -> [Task]
tasksForCurrentScreen model = tasksByScreen (screen model) (tasks model)


findIndexOfSelectedTaskId :: Maybe UUID -> [Task] -> Maybe Int
findIndexOfSelectedTaskId _       [] = Nothing
findIndexOfSelectedTaskId Nothing _  = Nothing
findIndexOfSelectedTaskId (Just selectedId) tasks =
  List.find (\t -> tId t == selectedId) tasks >>= (\t -> elemIndex t tasks)

previousTaskId :: Maybe UUID -> [Task] -> Maybe UUID
previousTaskId selectedTaskId tasks =
  fmap tId
    $ fmap (tasks !!)
    $ prevIndex tasks
    $ (findIndexOfSelectedTaskId selectedTaskId tasks)


nextTaskId :: Maybe UUID -> [Task] -> Maybe UUID
nextTaskId selectedTaskId tasks =
  fmap tId
    $ fmap (tasks !!)
    $ nextIndex tasks
    $ (findIndexOfSelectedTaskId selectedTaskId tasks)


selectPreviousTask :: Model -> Model
selectPreviousTask model =
  let tasks             = tasksForCurrentScreen model
      newSelectedTaskId = previousTaskId (selectedTaskId model) tasks
  in  model { selectedTaskId = newSelectedTaskId }


selectNextTask :: Model -> Model
selectNextTask model =
  let tasks             = tasksForCurrentScreen model
      newSelectedTaskId = nextTaskId (selectedTaskId model) tasks
  in  model { selectedTaskId = newSelectedTaskId }


deleteSelectedTask :: Model -> Model
deleteSelectedTask model = case selectedTaskId model of
  Nothing -> model
  Just selectedId ->
    let allTasks          = tasks model
        newTasks          = List.filter (\t -> tId t /= selectedId) $ allTasks
        newSelectedTaskId = nextTaskId (selectedTaskId model) allTasks
    in  model { tasks = newTasks, selectedTaskId = newSelectedTaskId }


selectNextScreen :: Model -> Model
selectNextScreen model =
  let newScreen = Screen.nextScreen (screen model)
  in  model { screen = newScreen }

selectPreviousScreen :: Model -> Model
selectPreviousScreen model =
  let newScreen = Screen.previousScreen (screen model)
  in  model { screen = newScreen }


toggleCompletionOfSelectedTask :: Model -> Model
toggleCompletionOfSelectedTask model = case selectedTaskId model of
  Nothing -> model
  Just selectedId ->
    let newTasks = List.map (Task.toggleCompletion selectedId) $ tasks model
    in  model { tasks = newTasks }

logAllCompleted :: Model -> Model
logAllCompleted model =
  let newTasks = List.map Task.logCompleted $ tasks model
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
awaitingCommand model (VtyEvent (EvKey (KChar 'J') [])) =
  M.continue $ selectNextScreen model
awaitingCommand model (VtyEvent (EvKey (KChar 'K') [])) =
  M.continue $ selectPreviousScreen model
awaitingCommand model (VtyEvent (EvKey (KChar 'L') [])) =
  M.continue $ logAllCompleted model
awaitingCommand model (VtyEvent (EvKey (KChar 'q') [])) = M.halt model
awaitingCommand model (VtyEvent (EvKey KEsc [])) = M.halt model
awaitingCommand model _ = M.continue model


screenToTaskDue :: Screen -> Task.Due
screenToTaskDue Screen.Inbox    = Task.Undecided
screenToTaskDue Screen.Today    = Task.Today
screenToTaskDue Screen.Upcoming = Task.Today
screenToTaskDue Screen.Anytime  = Task.Unscheduled
screenToTaskDue Screen.Someday  = Task.Someday


update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update model evt = case mode model of
  NewTask editor -> case evt of
    (VtyEvent (EvKey KEsc [])) -> M.continue $ model { mode = AwaitingCommand }
    (VtyEvent (EvKey KEnter []))
      -> let
           taskName        = T.pack $ unlines $ E.getEditContents editor
           allCurrentTasks = tasks model
         in
           if taskName == ""
             then M.continue $ model { mode = AwaitingCommand }
             else M.suspendAndResume $ do
               newTask <- Task.build (screenToTaskDue $ screen model) taskName
               return $ model { mode  = AwaitingCommand
                              , tasks = allCurrentTasks ++ [newTask]
                              }
    (VtyEvent ev) -> M.continue =<< fmap
      (\newEd -> model { mode = NewTask newEd })
      (E.handleEditorEvent ev editor)
    _ -> M.continue model
  EditTask id editor -> M.continue model
  AwaitingCommand    -> awaitingCommand model evt


-- VIEW


navItem :: String -> Widget Name
navItem item = C.vBox
  [C.vLimit 1 $ C.hBox [itemName, UI.fill, B.vBorder, count], C.hBox [B.hBorder]]
 where
  itemName = C.str item
  count    = C.str "   3 "


screenRow :: Screen -> Screen -> Widget Name
screenRow currentScreen screen =
  let formattedScreen = " " ++ Screen.format screen ++ " "
  in if currentScreen == screen
       then C.withBorderStyle UI.dashedBorder $ C.vBox
         [ C.vLimit 1 $ C.withAttr "selected" $ C.str $ formattedScreen
         , C.hBox [B.hBorder]
         ]
       else C.withBorderStyle UI.dashedBorder
         $ C.vBox [C.vLimit 1 $ C.str $ formattedScreen, C.hBox [B.hBorder]]


nav :: Screen -> Widget Name
nav currentScreen =
  C.withBorderStyle BS.unicodeRounded
    .  B.border
    $  C.hLimit 30
    $  C.vBox
    $  List.map (screenRow currentScreen) Screen.allScreens
    ++ [UI.fill]


isTaskSelected :: Maybe UUID -> Task -> Bool
isTaskSelected Nothing           _    = False
isTaskSelected (Just selectedId) task = tId task == selectedId


formatTask :: Task -> String
formatTask task = case status task of
  Incomplete -> UI.unchecked ++ " " ++ (unpack $ name task)
  Complete   -> UI.checked ++ " " ++ (unpack $ name task)
  Logged     -> UI.checked ++ " " ++ (unpack $ name task)


taskRow :: Maybe UUID -> Task -> Widget Name
taskRow selectedTaskId task = row
 where
  row = if isTaskSelected selectedTaskId task
    then C.withBorderStyle UI.dashedBorder $ C.vBox
      [ C.vLimit 1
      $  C.withAttr "selected"
      $  C.str
      $  " "
      ++ formatTask task
      ++ " "
      , C.hBox [B.hBorder]
      ]
    else C.withBorderStyle UI.dashedBorder $ C.vBox
      [C.vLimit 1 $ C.str $ " " ++ formatTask task ++ " ", C.hBox [B.hBorder]]


taskList :: Maybe UUID -> [Task] -> Widget Name
taskList selectedTaskId tasks =
  C.vBox $ (List.map (taskRow selectedTaskId) tasks) ++ [UI.fill]


contentTitle :: String -> Widget Name
contentTitle title =
  C.vBox [C.str " ", C.str (" " ++ title), C.str " ", B.hBorder]


content :: Screen -> Maybe UUID -> [Task] -> Widget Name
content currentScreen selectedTaskId tasks =
  C.withBorderStyle BS.unicodeRounded . B.border $ C.vBox
    [contentTitle $ Screen.format currentScreen, taskList selectedTaskId tasks]



workspace :: Model -> Widget Name
workspace model = C.hBox
  [ nav (screen model)
  , C.str " "
  , content (screen model) (selectedTaskId model) $ tasksForCurrentScreen model
  , C.str " "
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


render :: Model -> [Widget Name]
render model = [ui]
  where ui = C.joinBorders $ C.vBox [workspace model, cmdline model]



-- APP


app :: App Model e Name
app = App
  { appDraw         = render
  , appHandleEvent  = update
  , appAttrMap      = const UI.styles
  , appStartEvent   = return
  , appChooseCursor = neverShowCursor
  }


run :: IO ()
run = void $ initialState >>= M.defaultMain app

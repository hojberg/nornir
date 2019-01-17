{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.IO.Class
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

import           Util                           ( nextIndex
                                                , prevIndex
                                                )
import           Task                           ( Task(..)
                                                , TaskStatus(..)
                                                )
import qualified Task
import           DB
import qualified UI
import           Screen                         ( Screen(..) )
import qualified Screen

import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime

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
  , today          :: Day
  }


initialState :: [Task] -> IO Model
initialState tasks = do
  now      <- getCurrentTime
  timezone <- getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today'  = localDay zoneNow
  pure Model
    { tasks          = tasks
    , screen         = Screen.Today
    , mode           = AwaitingCommand
    , selectedTaskId = Nothing
    , today          = today'
    }


-- UPDATE

tasksForCurrentScreen :: Model -> [Task]
tasksForCurrentScreen model =
  let today'    = today model
      yesterday = addDays (-1) today'
      taskDue   = case screen model of
        Screen.Next      -> Task.Next
        Screen.Today     -> Task.OnDate today'
        Screen.Yesterday -> Task.OnDate yesterday

      forScreen t = due t == taskDue
  in  List.filter forScreen (tasks model)


tasksIndexToId :: [Task] -> Int -> UUID
tasksIndexToId tasks idx = tId $ tasks !! idx


previousTaskId :: Maybe UUID -> [Task] -> Maybe UUID
previousTaskId selectedTaskId tasks =
  fmap (tasksIndexToId tasks) $ prevIndex tasks $ Task.indexOfTaskId
    selectedTaskId
    tasks


nextTaskId :: Maybe UUID -> [Task] -> Maybe UUID
nextTaskId selectedTaskId tasks =
  fmap (tasksIndexToId tasks) $ nextIndex tasks $ Task.indexOfTaskId
    selectedTaskId
    tasks


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


deleteSelectedTask :: Model -> IO Model
deleteSelectedTask model = case selectedTaskId model of
  Nothing -> pure model
  Just selectedId ->
    let allTasks          = tasks model
        newTasks          = List.filter (\t -> tId t /= selectedId) allTasks
        newSelectedTaskId = nextTaskId (selectedTaskId model) allTasks
    in  do
          removeTask selectedId
          pure $ model { tasks = newTasks, selectedTaskId = newSelectedTaskId }


selectNextScreen :: Model -> Model
selectNextScreen model =
  let newScreen = Screen.nextScreen (screen model)
  in  model { screen = newScreen }


selectPreviousScreen :: Model -> Model
selectPreviousScreen model =
  let newScreen = Screen.previousScreen (screen model)
  in  model { screen = newScreen }


toggleCompletionOfSelectedTask :: Model -> IO Model
toggleCompletionOfSelectedTask model = case selectedTaskId model of
  Nothing -> pure model
  Just selectedId ->
    let newTasks    = List.map (Task.toggleCompletion selectedId) $ tasks model
        updatedTask = head $ List.filter (\t -> tId t == selectedId) newTasks
    in  do
          updateTask updatedTask
          pure model { tasks = newTasks }



awaitingCommand :: Model -> BrickEvent Name e -> EventM Name (Next Model)
awaitingCommand model event = case event of
  (VtyEvent (EvKey (KChar 'n') [])) ->
    M.continue $ model { mode = NewTask (E.editor TextBox Nothing "") }
  (VtyEvent (EvKey (KChar 'k') [])) -> M.continue $ selectPreviousTask model
  (VtyEvent (EvKey (KChar 'j') [])) -> M.continue $ selectNextTask model
  (VtyEvent (EvKey (KChar 'd') [])) ->
    liftIO (deleteSelectedTask model) >>= M.continue
  (VtyEvent (EvKey (KChar ' ') [])) ->
    liftIO (toggleCompletionOfSelectedTask model) >>= M.continue
  (VtyEvent (EvKey (KChar 'J') [])) -> M.continue $ selectNextScreen model
  (VtyEvent (EvKey (KChar 'K') [])) -> M.continue $ selectPreviousScreen model
  (VtyEvent (EvKey (KChar 'q') [])) -> M.halt model
  (VtyEvent (EvKey KEsc [])) -> M.halt model
  _ -> M.continue model


update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update model evt = case mode model of
  NewTask editor -> case evt of
    (VtyEvent (EvKey KEsc [])) -> M.continue $ model { mode = AwaitingCommand }
    (VtyEvent (EvKey KEnter []))
      -> let
           taskName        = T.pack $ unlines $ E.getEditContents editor
           allCurrentTasks = tasks model
           today'          = today model
           yesterday       = addDays (-1) today'

           dueForScreen    = case screen model of
             Screen.Next      -> Task.Next
             Screen.Today     -> Task.OnDate today'
             Screen.Yesterday -> Task.OnDate yesterday
         in
           if taskName == ""
             then M.continue $ model { mode = AwaitingCommand }
             else
               liftIO
                   (do
                     newTask <- Task.build dueForScreen taskName
                     DB.addTask newTask
                     return $ model { mode  = AwaitingCommand
                                    , tasks = allCurrentTasks ++ [newTask]
                                    }
                   )
                 >>= M.continue
    (VtyEvent ev) -> M.continue =<< fmap
      (\newEd -> model { mode = NewTask newEd })
      (E.handleEditorEvent ev editor)
    _ -> M.continue model
  EditTask id editor -> M.continue model
  AwaitingCommand    -> awaitingCommand model evt


-- VIEW


screenRow :: Screen -> Screen -> Widget Name
screenRow currentScreen screen =
  let formattedScreen = " " ++ Screen.format screen ++ " "
  in  if currentScreen == screen
        then C.withBorderStyle UI.dashedBorder $ C.vBox
          [ C.vLimit 1 $ C.withAttr "selected" $ C.str formattedScreen
          , C.hBox [B.hBorder]
          ]
        else C.withBorderStyle UI.dashedBorder
          $ C.vBox [C.vLimit 1 $ C.str formattedScreen, C.hBox [B.hBorder]]


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
  Incomplete -> UI.unchecked ++ " " ++ unpack (name task)
  Started    -> UI.dashed ++ " " ++ unpack (name task)
  Complete   -> UI.checked ++ " " ++ unpack (name task)


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
  C.vBox $ List.map (taskRow selectedTaskId) tasks ++ [UI.fill]


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
  in  C.withBorderStyle BS.unicodeRounded . B.border $ C.vLimit 1 $ C.center
        result


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
run =
  let dbCfg = Nothing
  in  void $ DB.init dbCfg >>= initialState >>= M.defaultMain app

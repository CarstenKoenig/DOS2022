module Components.Main
  ( Input
  , Output
  , Query
  , SlotType
  , component
  ) where

import Prelude

import Api.Tasks as Api
import Components.TodoItem as Item
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Halogen (ClassName(..), Component, ComponentSlot, HalogenM, Slot)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Models.Task (Task)
import Models.Task as Task
import Network.RemoteData (RemoteData)
import Network.RemoteData as RD
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Web

----------------------------------------------------------------------
-- Slots

type Slots :: Row Type
type Slots = (todoItem :: H.Slot Item.Query Item.Output Task.Id)

_todoItem = Proxy :: Proxy "todoItem"

type SlotType a = Slot Query Output a

----------------------------------------------------------------------
-- Basis-Definitionen der Komponente

type Input = Unit

type Query :: Type -> Type
type Query = Const Void

type Output = Void

-- | vereinfachendes Synnonym für HTML den diese Komponente produziert
type HtmlM m = HTML (ComponentSlot Slots m Action) Action

-- | vereinfachendes Synnonym für Effect-Handler den diese Komponente ausführen kann
type ThisHalogenM m a = HalogenM State Action Slots Output m a

----------------------------------------------------------------------
-- Komponente Definition
component :: forall m. MonadAff m => Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = initialize
        , finalize = finalize
        , receive = receiveInput
        }
    }
  where

  initialState :: Input -> State
  initialState _ =
    { tasks: RD.NotAsked
    , busyTasks: Set.empty
    , neuerTaskWirdEingefuegt: RD.NotAsked
    , taskWirdGeloescht: RD.NotAsked
    , inputNeu: ""
    }

  render :: State -> HtmlM m
  render = view

  handleAction :: Action -> ThisHalogenM m Unit
  handleAction = update

  handleQuery :: forall cont. Query cont -> ThisHalogenM m (Maybe cont)
  handleQuery _ = pure Nothing

  initialize :: Maybe Action
  initialize = Just Initializing

  finalize :: Maybe Action
  finalize = Nothing

  receiveInput :: Input -> Maybe Action
  receiveInput _ = Nothing

----------------------------------------------------------------------
-- Model 

type State =
  { tasks :: RemoteData String (Map Task.Id Task)
  , busyTasks :: Set Task.Id
  , neuerTaskWirdEingefuegt :: RemoteData String Unit
  , taskWirdGeloescht :: RemoteData String Unit
  , inputNeu :: String
  }

anzahlOffene :: State -> Maybe Int
anzahlOffene state =
  state.tasks
    # RD.toMaybe
    # map (Map.filter (\task -> not task.taskCompleted))
    # map (Map.size)

isBusy :: State -> Boolean
isBusy state =
  RD.isLoading state.tasks
    || RD.isLoading state.neuerTaskWirdEingefuegt
    || RD.isLoading state.taskWirdGeloescht
    || not (Set.isEmpty state.busyTasks)

----------------------------------------------------------------------
-- Actions / Update

data Action
  = Initializing
  | ItemOutput Task.Id Item.Output
  | InputNeuLostFocus
  | InputNeuChanged String
  | InputNeuKeydown KeyboardEvent

update :: forall m. MonadAff m => Action -> ThisHalogenM m Unit
update = case _ of
  Initializing -> do
    H.modify_ _ { tasks = RD.Loading }
    res <- (RD.fromEither <<< map extractMap) <$> Api.getAll
    H.modify_ _ { tasks = res }
  ItemOutput _ (Item.TaskChanged changedTask) -> do
    let
      updateTask = Map.insert changedTask.taskId changedTask
    H.modify_ (\state -> state { tasks = map updateTask state.tasks })
  ItemOutput taskId (Item.IsBusy busy) -> do
    H.modify_ (\state -> state { busyTasks = (if busy then Set.insert else Set.delete) taskId state.busyTasks })
  ItemOutput taskId Item.DeleteTask -> do
    H.modify_ _ { taskWirdGeloescht = RD.Loading }
    result <- Api.deleteTask taskId
    H.modify_ _ { taskWirdGeloescht = RD.fromEither result }
    case result of
      Left err -> do
        error err
      Right _ ->
        H.modify_
          ( \state ->
              state { inputNeu = "", tasks = state.tasks # map (Map.delete taskId) }
          )
  InputNeuLostFocus ->
    H.modify_ _ { inputNeu = "" }
  InputNeuChanged neuText ->
    H.modify_ _ { inputNeu = neuText }
  InputNeuKeydown keyEvent ->
    case Web.key keyEvent of
      "Escape" ->
        H.modify_ _ { inputNeu = "" }
      "Enter" -> do
        oldState <- H.get
        -- Falls eine neue Aufgabe eingetragen ist
        unless (oldState.inputNeu == "") do
          H.modify_ _ { neuerTaskWirdEingefuegt = RD.Loading }
          -- schicke Anfrage ans Backend
          result <- Api.newTask { description: oldState.inputNeu }
          H.modify_ _ { neuerTaskWirdEingefuegt = void $ RD.fromEither result }
          case result of
            Left err -> do
              error err
            Right insertedTask -> do
              H.modify_
                ( \state ->
                    let
                      insertNewTask dict =
                        Map.insert insertedTask.taskId insertedTask dict
                    in
                      state { inputNeu = "", tasks = state.tasks # map insertNewTask }
                )
          H.modify_ _ { inputNeu = "" }
      _ -> pure unit
  where
  extractMap :: Array Task -> Map Task.Id Task
  extractMap =
    Map.fromFoldable <<< map (\task -> Tuple task.taskId task)

----------------------------------------------------------------------
-- Rendern / View Bereich

view :: forall m. MonadAff m => State -> HtmlM m
view state =
  HH.div
    [ HP.class_ (ClassName "todoapp") ]
    [ viewHeader state
    , viewMain state
    , viewFooter state
    ]

viewHeader :: forall m. State -> HtmlM m
viewHeader state =
  HH.header
    [ HP.class_ (ClassName "header") ]
    [ HH.h1_ [ HH.text "Aufgaben" ]
    , viewInputNewTodo state
    ]

viewInputNewTodo :: forall m. State -> HtmlM m
viewInputNewTodo state =
  HH.input
    [ HP.class_ (ClassName "new-todo")
    , HP.placeholder "Was steht an?"
    , HP.value state.inputNeu
    , HP.disabled (isBusy state)
    , HE.onValueInput InputNeuChanged
    , HE.onKeyDown InputNeuKeydown
    , HE.onBlur (\_ -> InputNeuLostFocus)
    ]

viewMain :: forall m. MonadAff m => State -> HtmlM m
viewMain state =
  HH.section
    [ HP.class_ (ClassName "main") ]
    ( Array.concat
        [ viewToggleAll state
        , viewTodoListe state
        ]
    )

viewToggleAll :: forall m. State -> Array (HtmlM m)
viewToggleAll _ =
  [ HH.input
      [ HP.id "toggle-all"
      , HP.type_ InputCheckbox
      , HP.class_ (ClassName "toggle-all")
      ]
  , HH.label
      [ HP.for "toggle-all" ]
      []
  ]

viewTodoListe :: forall m. MonadAff m => State -> Array (HtmlM m)
viewTodoListe state =
  [ HH.ul
      [ HP.class_ (ClassName "todo-list") ]
      ( state.tasks
          # RD.withDefault Map.empty
          # Map.toUnfoldable
          # map (\(Tuple _ task) -> viewTodoItem state task)
      )
  ]

viewTodoItem :: forall m. MonadAff m => State -> Task -> HtmlM m
viewTodoItem state task =
  HH.slot _todoItem task.taskId Item.component { task: task, parentBusy: isBusy state } (ItemOutput task.taskId)

viewFooter :: forall m. State -> HtmlM m
viewFooter state =
  HH.footer
    [ HP.class_ (ClassName "footer") ]
    [ viewCount state
    ]

viewCount :: forall m. State -> HtmlM m
viewCount state =
  case anzahlOffene state of
    Just offene ->
      HH.span
        [ HP.class_ (ClassName "todo-count") ]
        [ HH.strong_ [ HH.text $ show offene ]
        , HH.span_ [ HH.text " übrig" ]
        ]
    Nothing ->
      HH.text ""
module Components.TodoItem
  ( Input
  , Output(..)
  , Query
  , SlotType
  , component
  ) where

import Prelude

import Api.Tasks as Api
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error)
import Halogen (ClassName(..), Component, ComponentSlot, HalogenM, RefLabel(..), Slot)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Models.Task (Task, ModifyTask)
import Models.Task as Task
import Network.RemoteData (RemoteData)
import Network.RemoteData as RD
import Web.HTML.HTMLElement as Html
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Web

----------------------------------------------------------------------
-- Komponenten - Basis Definitionen
type Input = { task :: Task, parentBusy :: Boolean }

type Query :: Type -> Type
type Query = Const Void

data Output
  = TaskChanged Task
  | IsBusy Boolean
  | DeleteTask

type Slots :: Row Type
type Slots = ()

type SlotType a = Slot Query Output a

-- | vereinfachendes Synnonym für HTML den diese Komponente produziert
type HtmlM m = HTML (ComponentSlot Slots m Action) Action

-- | vereinfachendes Synnonym für Effect-Handler den diese Komponente ausführen kann
type ThisHalogenM m a = HalogenM State Action Slots Output m a

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
  initialState input =
    { task: input.task
    , parentBusy: input.parentBusy
    , editing: false
    , editText: ""
    , saveChanges: RD.NotAsked
    }

  render :: State -> HtmlM m
  render = view

  handleAction :: Action -> ThisHalogenM m Unit
  handleAction = update

  handleQuery :: forall cont. Query cont -> ThisHalogenM m (Maybe cont)
  handleQuery _ = pure Nothing

  initialize :: Maybe Action
  initialize = Nothing

  finalize :: Maybe Action
  finalize = Nothing

  receiveInput :: Input -> Maybe Action
  receiveInput = Just <<< InputChanged

----------------------------------------------------------------------
-- Zustand-Model

type State =
  { parentBusy :: Boolean
  , task :: Task
  , editing :: Boolean
  , editText :: String
  , saveChanges :: RemoteData String Unit
  }

isBusy :: State -> Boolean
isBusy state =
  RD.isLoading state.saveChanges
    || state.parentBusy

----------------------------------------------------------------------
-- Zustandsänderungen

data Action
  = InputChanged Input
  | ToggleCompleted Boolean
  | SwitchToEdit
  | EditTextChanged String
  | EditInputKeydown KeyboardEvent
  | EditLostFocus
  | DeleteItem

update :: forall m. MonadAff m => Action -> ThisHalogenM m Unit
update = case _ of
  InputChanged newInput ->
    H.modify_ _
      { task = newInput.task
      , parentBusy = newInput.parentBusy
      }
  ToggleCompleted comp -> do
    commitChanges (\state -> Task.setComplete comp state.task)

  SwitchToEdit -> do
    H.modify_
      ( \state -> state
          { editing = true
          , editText = state.task.taskDescription
          }
      )
    focusElement refEdit
  EditTextChanged newText ->
    H.modify_ (_ { editText = newText })
  EditInputKeydown keydownEvent ->
    case Web.key keydownEvent of
      "Escape" -> exitEditing
      "Enter" -> acceptEdit
      _ -> pure unit
  EditLostFocus ->
    exitEditing
  DeleteItem ->
    H.raise DeleteTask

exitEditing :: forall m. ThisHalogenM m Unit
exitEditing =
  H.modify_ _ { editing = false }

acceptEdit :: forall m. MonadAff m => ThisHalogenM m Unit
acceptEdit =
  commitChanges (\state -> Task.setDescription state.editText state.task)

commitChanges :: forall m. MonadAff m => (State -> Tuple Task ModifyTask) -> ThisHalogenM m Unit
commitChanges doChanges = do
  oldState <- H.get
  let (Tuple modifiedTask modified) = doChanges oldState
  H.raise (IsBusy true)
  H.modify_ _ { task = modifiedTask, saveChanges = RD.Loading }
  res <- Api.patchTask modifiedTask.taskId modified
  H.modify_ _ { saveChanges = void $ RD.fromEither res }
  case res of
    Left err -> do
      error err
      H.put oldState
    Right savedTask -> do
      curState <- H.get
      update (InputChanged { task: savedTask, parentBusy: curState.parentBusy })
      H.raise (TaskChanged savedTask)
  H.raise (IsBusy false)

refEdit :: RefLabel
refEdit = RefLabel "inputEdit"

----------------------------------------------------------------------
-- Rendern / View Bereich

-- | erzeugt eine HTML Repräsentation des Zustands
view :: forall m. State -> HtmlM m
view state =
  HH.li
    [ HP.classes classes ]
    [ HH.div
        [ HP.class_ (ClassName "view") ]
        [ HH.input
            [ HP.class_ (ClassName "toggle")
            , HP.type_ InputCheckbox
            , HP.checked state.task.taskCompleted
            , HP.disabled (isBusy state)
            , HE.onChecked ToggleCompleted
            ]
        , HH.label
            [ HE.onDoubleClick (\_ -> SwitchToEdit) ]
            [ HH.text state.task.taskDescription ]
        , HH.button
            [ HP.class_ (ClassName "destroy")
            , HP.disabled (isBusy state)
            , HE.onClick (\_ -> DeleteItem)
            ]
            []
        ]
    , HH.input
        [ HP.class_ (ClassName "edit")
        , HP.ref refEdit
        , HP.value state.editText
        , HP.disabled (isBusy state)
        , HE.onValueInput EditTextChanged
        , HE.onBlur (\_ -> EditLostFocus)
        , HE.onKeyDown EditInputKeydown
        ]
    ]
  where
  classes =
    Array.mapMaybe
      identity
      [ if state.task.taskCompleted then Just (ClassName "completed") else Nothing
      , if state.editing then Just (ClassName "editing") else Nothing
      ]

focusElement :: forall m. MonadEffect m => RefLabel -> ThisHalogenM m Unit
focusElement ref = do
  element <- H.getHTMLElementRef ref
  for_ element $ (liftEffect <<< Html.focus)

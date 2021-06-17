module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array (length, take)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (hush)
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.Time (Time(..))
import Data.Time as Time
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Aff as Aff
import Effect.Now (now)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Halogen.Query.EventSource as ES.EventSource
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage)
import Web.Storage.Storage as Storage

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body


type State = {
  isRunning :: Maybe { prevTick :: Instant }
, time :: MTime
, times :: Array MTime
, m_storage :: Maybe Storage
}

type MTime = Number

initialState :: State
initialState = {
  isRunning: Nothing
, time: 0.0
, times: []
, m_storage: Nothing
}

parseTimes :: Maybe String -> Maybe (Array MTime)
parseTimes m_timesS = do
  timesS <- m_timesS
  json   <- hush $ parseJson timesS
  times  <- hush $ decodeJson json
  pure times

data Action 
  = Init
  | Tick
  | Start
  | Pause
  | ResetHistory
  | Done

tickPeriodMs :: Milliseconds
tickPeriodMs = Milliseconds 50.0

component :: forall output input query. H.Component HH.HTML query input output Aff
component =
  H.mkComponent
    { 
      initialState: \a -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction, initialize = Just Init }
      -- { handleAction = handleAction }
    }
  where
  render {isRunning, time, times} = 
    HH.table_ $
      [
        HH.tr_ [ HH.td [] [startPauseButton] ]
      , HH.tr_ [ HH.td [] [HH.text $ showTime time] ]
      , HH.tr_ [ HH.td [] [doneButton] ]
      , HH.tr_ 
        [ 
          HH.td [] [HH.text $ "Avg of last " <> show avg_n5 <> ":"]
        , HH.td [] [HH.text $ if avg_n5 > 0 then showTime avg_time5 else "N/A"]
        ]
      , HH.tr_ 
        [ 
          HH.td [] [HH.text $ "Avg of all " <> show avg_n <> ":"]
        , HH.td [] [HH.text $ if avg_n > 0 then showTime avg_time else "N/A"]
        ]
      , HH.tr_ [ HH.td [] [resetHistoryButton] ]
      , HH.tr_ [ HH.td [] [HH.text $ "Recent times:"] ]
      ]
      <> timesRows
    where
    timesRows = map timeRow times
    timeRow t = HH.tr_ [ HH.td_ [HH.text $ showTime t] ]

    avg_n = length times
    avg_n5 = min 5 avg_n
    avg_time = (sum times) / (Int.toNumber avg_n)
    avg_time5 = (sum (take avg_n5 times)) / (Int.toNumber avg_n5)

    actionButton {action, label} = 
      HH.button [HE.onClick (\ _ -> Just action)] [HH.text label]
    startPauseButton 
      | isJust isRunning = actionButton {action: Pause, label: "Pause"}
      | otherwise        = actionButton {action: Start, label: "Start"}
    doneButton           = actionButton {action: Done , label: "Done"}
    resetHistoryButton   = actionButton {action: ResetHistory , label: "Reset history"}

  handleAction = case _ of
    Init -> do
      -- get hold of this window's local storage:
      storage <- liftEffect $ window >>= localStorage
      times <- readLocalStorage {storage, default: []}
      H.modify_ $ _ { m_storage = Just storage, times = times }
      
      void $ H.subscribe ticker

    Start -> do
      prevTick <- liftEffect now
      H.modify_ $ _ { isRunning = Just {prevTick} }

    Pause -> do
      handleAction Tick -- count the time since previous Tick
      H.modify_ $ _ { isRunning = Nothing }

    Done -> do
      handleAction Tick -- count the time since previous Tick
      H.modify_ \s -> s { isRunning = Nothing, time = 0.0, times = [s.time] <> s.times }
      updateLocalStorage

    ResetHistory -> do
      H.modify_ $ _ { times = [] }
      updateLocalStorage

    Tick -> do
      {isRunning} <- H.get
      case isRunning of
        Just {prevTick} -> do
          thisTick <- liftEffect now
          let (duration :: Milliseconds) = DateTime.diff (toDateTime thisTick) (toDateTime prevTick)
          let extendTime time = case (Milliseconds time) <> duration of Milliseconds m -> m
          H.modify_ \ s -> s 
            { isRunning = Just {prevTick: thisTick}
            , time = extendTime s.time }
        _ -> pure unit
    -- _ -> pure unit

  updateLocalStorage = do
      {m_storage, times} <- H.get
      case m_storage of
        Nothing -> pure unit
        Just s -> liftEffect $ Storage.setItem "times" (stringify $ encodeJson times) s

  readLocalStorage {storage, default} = do
    -- attempt to get history from local storage:
    m_timesS <- liftEffect $ Storage.getItem "times" storage
    let m_times = parseTimes m_timesS
    pure $ maybe default identity m_times

ticker :: ES.EventSource Aff Action
ticker = ES.EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay tickPeriodMs
    ES.EventSource.emit emitter Tick

  pure $ ES.EventSource.Finalizer do
    Aff.killFiber (error "ticker: Event source finalized") fiber

showTime :: MTime -> String
showTime mt =
  let Tuple _ (Time h m s _) = Time.adjust (Milliseconds mt) bottom in
  show (fromEnum h) <> ":" <> showPad2 (fromEnum m) <> ":" <> showPad2 (fromEnum s)

showPad2 :: forall t. Show t => t -> String
showPad2 n = aux  
  where
  aux 
    | nSlen == 1 = "0" <> nS
    | otherwise = nS
  nS = show n
  nSlen = String.length nS


module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (length, take)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Enum (fromEnum)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
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

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body


type State = {
  isRunning :: Maybe { prevTick :: Instant }
, time :: MTime
, times :: Array MTime
}

type MTime = Milliseconds

initialState :: State
initialState = {
  isRunning: Nothing
, time: Milliseconds 0.0
, times: []
}

data Action 
  = Init
  | Tick
  | Start
  | Pause
  -- | Reset
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
      , HH.tr_ [ HH.td [] [HH.text $ "Recent times:"] ]
      ]
      <> timesRows
    where
    timesRows = map timeRow times
    timeRow t = HH.tr_ [ HH.td_ [HH.text $ showTime t] ]

    avg_n = length times
    avg_n5 = min 5 avg_n
    avg_time = Milliseconds $ (sum timesN) / (Int.toNumber avg_n)
    avg_time5 = Milliseconds $ (sum (take avg_n5 timesN)) / (Int.toNumber avg_n5)
    timesN = map (\(Milliseconds m) -> m) times

    actionButton {action, label} = 
      HH.button [HE.onClick (\ _ -> Just action)] [HH.text label]
    startPauseButton 
      | isJust isRunning = actionButton {action: Pause, label: "Pause"}
      | otherwise        = actionButton {action: Start, label: "Start"}
    doneButton           = actionButton {action: Done , label: "Done"}
    

  handleAction = case _ of
    Init -> do
      void $ H.subscribe ticker

    Start -> do
      prevTick <- liftEffect now
      H.modify_ $ _ { isRunning = Just {prevTick} }

    Pause -> do
      handleAction Tick -- count the time since previous Tick
      H.modify_ $ _ { isRunning = Nothing }

    Done -> do
      handleAction Tick -- count the time since previous Tick
      H.modify_ \s -> s { isRunning = Nothing, time = Milliseconds 0.0, times = [s.time] <> s.times }

    Tick -> do
      {isRunning} <- H.get
      case isRunning of
        Just {prevTick} -> do
          thisTick <- liftEffect now
          let (duration :: Milliseconds) = DateTime.diff (toDateTime thisTick) (toDateTime prevTick)
          H.modify_ \ s -> s 
            { isRunning = Just {prevTick: thisTick}
            , time = s.time <> duration }
        _ -> pure unit
    -- _ -> pure unit


ticker :: ES.EventSource Aff Action
ticker = ES.EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay tickPeriodMs
    ES.EventSource.emit emitter Tick

  pure $ ES.EventSource.Finalizer do
    Aff.killFiber (error "ticker: Event source finalized") fiber

showTime :: MTime -> String
showTime mt =
  let Tuple _ (Time h m s _) = Time.adjust mt bottom in
  show (fromEnum h) <> ":" <> showPad2 (fromEnum m) <> ":" <> showPad2 (fromEnum s)

showPad2 :: forall t. Show t => t -> String
showPad2 n = aux  
  where
  aux 
    | nSlen == 1 = "0" <> nS
    | otherwise = nS
  nS = show n
  nSlen = String.length nS


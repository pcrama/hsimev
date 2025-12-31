-- | Simulation of a single charging session with pre-defined external input events
module FakeSimulation (SimulationTrace (..), fakeSimulation) where

import ChargingStation
import Control.Monad.Writer (MonadWriter, tell)
import Data.List (foldl')

fakeSimulation :: (Session -> Duration -> InputEvent i -> (OutputEvent [] e, Session)) -> Session -> Timestamp -> [(Duration, i)] -> Duration -> ([(Timestamp, Session)], [(Timestamp, e)])
fakeSimulation stepper startSession t0 safeCalendar totalDuration =
  let SimulationTrace sessionTrace outputTrace = fst $ goSimulation (SimState {tick = t0, session = startSession}) initialCalendar
   in (fmap (\(SimState {tick, session}) -> (tick, session)) sessionTrace, outputTrace)
  where
    lastTimestamp = totalDuration `after` t0
    initialCalendar =
      reverse $
        filter (\InputEvent {ieTick} -> ieTick <= lastTimestamp) $
          snd $
            foldl'
              (\result@(t, as) (dur, ie) -> if t > lastTimestamp then result else let t1 = dur `after` t in (t1, InputEvent {ieTick = t1, ieTrigger = Just ie} : as))
              (t0, [])
              safeCalendar
    -- goSimulation ::
    --   (Show i) =>
    --   -- | step SimState's session from input event's ieTick to ieTick + duration
    --   (Session -> Duration -> InputEvent i -> (OutputEvent [] e, Session)) ->
    --   -- | current simulator state (contains current timestamp)
    --   SimState ->
    --   -- | calendar of input events (known beforehand because this is a fake simulation)
    --   [InputEvent i] ->
    --   -- | end of simulation
    --   Timestamp ->
    --   (SimulationTrace e, ())
    goSimulation startState@(SimState {tick}) calendar = do
      tell $ SimulationTrace [startState] []
      simulate stepper deliverEvent recurse startState calendarHead
      where
        defaultNextTimestamp = min lastTimestamp $ minutes 1 `after` tick
        deliverEvent evt = tell $ SimulationTrace [] [(nextTimestamp, evt)]
        (calendarHead@(InputEvent {ieTick = nextTimestamp}), calendarTail) = case calendar of
          hd@(InputEvent {ieTick = hdTick}) : tl | hdTick <= defaultNextTimestamp -> (hd, tl)
          _ -> (InputEvent {ieTick = defaultNextTimestamp, ieTrigger = Nothing}, calendar)
        -- recurse ::
        --   -- | next state = first state of the rest of the simulation
        --   SimState ->
        --   -- | next input event requested by the `stepper`, i.e. still to merge with calendarTail
        --   InputEvent i -> m ()
        recurse st ie
          | tick >= lastTimestamp = return ()
          | otherwise = goSimulation st (mergeIntoCalendar calendarTail ie)
        mergeIntoCalendar [] ie = [ie]
        mergeIntoCalendar (ca@InputEvent {ieTick = caTick, ieTrigger = caTrigger} : ccaa) ie@InputEvent {ieTick, ieTrigger}
          | caTick < ieTick = ca : mergeIntoCalendar ccaa ie
          | ieTick < caTick = ie : ca : ccaa
          | otherwise = case (caTrigger, ieTrigger) of
              (Nothing, _) -> ie : ccaa
              (_, Nothing) -> ca : ccaa
              (Just _, Just _) -> ca : (mergeIntoCalendar ccaa $ InputEvent {ieTick = milliseconds 1 `after` ieTick, ieTrigger = ieTrigger})

data SimulationTrace e = SimulationTrace [SimState] [(Timestamp, e)]
  deriving stock (Show)

instance Semigroup (SimulationTrace e) where
  (SimulationTrace lftStates lftEvents) <> (SimulationTrace rgtStates rgtEvents) = SimulationTrace (lftStates <> rgtStates) $ lftEvents <> rgtEvents

instance Monoid (SimulationTrace e) where
  mempty = SimulationTrace [] []

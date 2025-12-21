module ChargingStation
  ( stepSimulation,
    SessionTarget (..),
    Timestamp (..),
    Duration,
    MeterValues (..),
    MeterValuesStateMachine (..),
    OutputEvent (..),
    Phases (..),
    SessionState (..),
    SessionConfiguration (..),
    Session (..),
    SimState (..),
    seconds,
    minutes,
    durationUntil,
    after,
    chargeEfficientlyUntil80Percent,
    getInstantaneousCurrentMaxUntil80Percent,
  )
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word64, Word8)

-- | [ms] since some imaginary epoch
newtype Timestamp = Timestamp Word64
  deriving (Show, Eq, Ord)

newtype Duration
  = Duration
      -- | [ms]
      Word64
  deriving (Show, Eq, Ord)

instance Semigroup Duration where
  (Duration lf) <> (Duration rg) = Duration $ lf + rg

instance Monoid Duration where
  mempty = Duration 0

after :: Duration -> Timestamp -> Timestamp
(Duration dur) `after` (Timestamp ts) = Timestamp $ ts + dur

before :: Duration -> Timestamp -> Timestamp
(Duration dur) `before` (Timestamp ts)
  | ts >= dur = Timestamp $ ts - dur
  | otherwise = error "Timestamp underflow"

durationUntil :: Timestamp -> Timestamp -> Duration
durationUntil (Timestamp from_) (Timestamp to_)
  | from_ <= to_ = Duration $ to_ - from_
  | otherwise = error "Time must always go forward"

clampingDurationUntil :: Timestamp -> Timestamp -> Duration
clampingDurationUntil (Timestamp from_) (Timestamp to_)
  | from_ <= to_ = Duration $ to_ - from_
  | otherwise = Duration 0

secondsFrom :: Duration -> Double
secondsFrom (Duration dur) = fromIntegral dur / 1000.0

minutes :: Word64 -> Duration
minutes = Duration . (* 60000)

seconds :: Word64 -> Duration
seconds = Duration . (* 1000)

milliseconds :: Word64 -> Duration
milliseconds = Duration

data SessionTarget
  = -- | driver wants to leave at a fixed time
    LeaveAtTick !Timestamp
  | -- | driver wants to leave at the earliest after the given time but only if their charge level is sufficient
    LeaveAfterBoth !Timestamp !Double
  | -- | driver will stay until the given time or leave as soon as their battery has reached that charge level
    LeaveAfterEither !Timestamp !Double
  | -- | drive will stay until their battery has reached that charge level
    LeaveAtLevel !Double
  deriving (Show, Eq)

data Phases
  = -- | Mono on 1st phase
    R
  | -- | Mono on 2nd phase
    S
  | -- | Mono on 3rd phase
    T
  | -- | Two phases (1+2)
    RS
  | -- | Two phases (2+3)
    ST
  | -- | Two phases (1+3)
    RT
  | -- | Three phases
    RST
  deriving (Show, Eq)

data SessionState
  = Available
  | Preparing
  | Charging
      { -- | how many [Wh] are in the car's battery
        batteryLevel :: !Double,
        -- | how many [Wh] were taken from the grid
        energyDelivered :: !Double,
        -- | how many [A] the EV should use
        currentOffered :: !Double,
        transactionId :: !Word64,
        meterValuesStateMachine :: !MeterValuesStateMachine
      }
  | Finishing
  deriving (Show, Eq)

data MeterValuesStateMachine
  = -- | Meter values were sampled and sent, we're waiting for the next time to sample new values
    NextMeterValueSampleDue !Timestamp
  | -- | Meter values are sampled, we're waiting for the right moment to send them
    Sampled !Timestamp !MeterValues
  deriving (Show, Eq)

data MeterValues = MeterValues
  { mvTransactionId :: !Word64,
    mvStationId :: !Text,
    mvConnectorId :: !Word8,
    mvTimestamp :: !Timestamp,
    mvCurrents :: !(Double, Double, Double),
    mvOfferedCurrent :: !Double,
    mvEnergyDelivered :: !Double
  }
  deriving (Show, Eq)

data SessionConfiguration = SessionConfiguration
  { -- | how many [Wh] "fit" in the car's battery
    batteryCapacity :: !Double,
    sessionTarget :: !SessionTarget,
    -- | update session state to account for passing of time
    charge :: SessionState -> Duration -> Timestamp -> (OutputEvent [] MeterValues, SessionState),
    getInstantaneousCurrent :: Timestamp -> SessionState -> (Double, Double, Double),
    phases :: !Phases,
    stationId :: !Text,
    connectorId :: !Word8,
    meterValuesPeriodicity :: !Duration
  }

data Session = Session !SessionConfiguration !SessionState

data SimState = SimState {tick :: !Timestamp, session :: !Session}

data InputEvent a = InputEvent
  { ieTick :: !Timestamp,
    ieTrigger :: !(Maybe a)
  }
  deriving (Show, Eq)

data OutputEvent w b
  = NoEvent
  | OutputEvent
      { -- | Request for next step no later than after this duration
        oeNext :: !Duration,
        -- | Produce those output events `b` right now and next step no later than after this duration
        oeEvent :: !(w b)
      }
  deriving (Show, Eq)

instance (Semigroup (w b)) => Semigroup (OutputEvent w b) where
  lf <> NoEvent = lf
  NoEvent <> rg = rg
  lf@(OutputEvent {oeNext = lfNxt, oeEvent = lfEvts}) <> rg@(OutputEvent {oeNext = rgNxt, oeEvent = rgEvts}) =
    OutputEvent
      { oeNext = min lfNxt rgNxt,
        oeEvent = lfEvts <> rgEvts
      }

instance (Semigroup (w b)) => Monoid (OutputEvent w b) where
  mempty = NoEvent

tellNextSimulationStep :: (Monoid (w b), MonadWriter (OutputEvent w b) m) => Duration -> m ()
tellNextSimulationStep dur = tell $ OutputEvent {oeNext = dur, oeEvent = mempty}

tellNextSimulationStepAndEvent :: (MonadWriter (OutputEvent [] b) m) => Duration -> b -> m b
tellNextSimulationStepAndEvent dur evt = do
  tell $ OutputEvent {oeNext = dur, oeEvent = [evt]}
  return evt

countPhases :: (Double, Double, Double) -> Int
countPhases (x, y, z) = (if x > 0.0 then 1 else 0) + (if y > 0.0 then 1 else 0) + (if z > 0.0 then 1 else 0)

chargeEfficientlyUntil80Percent ::
  -- | charging session configuration
  SessionConfiguration ->
  -- | current charging session state
  SessionState ->
  -- | duration of the time step to advance the session state
  Duration ->
  -- | timestamp of the new session state (i.e. timestamp of last session state + time step)
  Timestamp ->
  -- | new session state
  (OutputEvent [] MeterValues, SessionState)
chargeEfficientlyUntil80Percent _ Available _ _ = return Available
chargeEfficientlyUntil80Percent _ Preparing _ _ = return Preparing
chargeEfficientlyUntil80Percent
  (SessionConfiguration {getInstantaneousCurrent, sessionTarget, batteryCapacity})
  sessionState@(Charging {energyDelivered, batteryLevel, currentOffered})
  dt
  nextTime = do
    let
    case sessionTarget of
      LeaveAtTick ts ->
        if ts <= nextTime then return Available else accumulateEnergy $ nextTime `durationUntil` ts
      LeaveAfterBoth ts targetLevel ->
        if (ts <= nextTime) && (targetLevel <= newLevel) then return Available else accumulateEnergy $ nextTime `durationUntil` ts
      LeaveAfterEither ts targetLevel ->
        if (ts <= nextTime) || (targetLevel <= newLevel)
          then return Available
          else accumulateEnergy $ if ts <= nextTime then defaultNextSimulationStep else nextTime `durationUntil` ts
      LeaveAtLevel targetLevel ->
        if targetLevel <= newLevel then return Available else accumulateEnergy defaultNextSimulationStep
    where
      instantaneousCurrent = getInstantaneousCurrent (dt `before` nextTime) sessionState
      defaultNextSimulationStep = minutes 5
      voltage = 230
      sumOfCurrents = let (x, y, z) = instantaneousCurrent in x + y + z
      deltaEnergyDelivered = (voltage * sumOfCurrents * secondsFrom dt) / 3600.0 -- 3600 to convert from [J] to [Wh]
      batteryPercentage = batteryLevel / batteryCapacity
      chargingEfficiency = if batteryPercentage < 0.8 then 0.85 else 0.75
      newLevel = batteryLevel + deltaEnergyDelivered * chargingEfficiency
      newEnergyDelivered = energyDelivered + deltaEnergyDelivered
      accumulateEnergy nextStep = do
        tellNextSimulationStep nextStep
        return sessionState {energyDelivered = newEnergyDelivered, batteryLevel = newLevel}

getInstantaneousCurrentMaxUntil80Percent ::
  Double ->
  Double ->
  SessionConfiguration ->
  -- | timestamp, ignored
  Timestamp ->
  SessionState ->
  (Double, Double, Double)
getInstantaneousCurrentMaxUntil80Percent _ _ _ _ Available = (0.0, 0.0, 0.0)
getInstantaneousCurrentMaxUntil80Percent _ _ _ _ Preparing = (0.0, 0.0, 0.0)
getInstantaneousCurrentMaxUntil80Percent _ _ _ _ Finishing = (0.0, 0.0, 0.0)
getInstantaneousCurrentMaxUntil80Percent
  maxCurrent
  offset
  (SessionConfiguration {batteryCapacity, phases})
  _
  (Charging {batteryLevel, currentOffered}) =
    case phases of
      R -> (cappedCurrent, 0.0, 0.0)
      S -> (0.0, cappedCurrent, 0.0)
      T -> (0.0, 0.0, cappedCurrent)
      RS -> (cappedCurrent, cappedCurrent, 0.0)
      ST -> (0.0, cappedCurrent, cappedCurrent)
      RT -> (cappedCurrent, 0.0, cappedCurrent)
      RST -> (cappedCurrent, cappedCurrent, cappedCurrent)
    where
      batteryPercentage = batteryLevel / batteryCapacity
      knee = 0.8 -- start decreasing charging current from maxCurrent down to minCurrent when batteryCapacity > knee
      minCurrent = 1.0
      batteryCurrent = if batteryPercentage < knee then maxCurrent else minCurrent + (maxCurrent - minCurrent) * (1.0 - batteryPercentage) / (1 - knee)
      cappedCurrent = if currentOffered == 0 then 0 else max 0.1 $ min batteryCurrent $ currentOffered - offset

stepSimulation :: SimState -> Timestamp -> (OutputEvent [] MeterValues, SimState)
stepSimulation (SimState {tick = t0}) ts
  | ts <= t0 = error "Can't simulate time backwards"
stepSimulation
  simState@(SimState {tick = t0, session = session@(Session sessionConfiguration sessionState@(Charging {}))})
  ts = do
    newState <- stepSessionState session (t0 `durationUntil` ts) $ InputEvent {ieTick = ts, ieTrigger = Nothing}
    return simState {tick = ts, session = Session sessionConfiguration newState}
stepSimulation simState ts = return simState {tick = ts}

data SetChargingProfile = SetChargingProfile
  { -- | transactionId for which the setpoint should be applied
    scpTransactionId :: !Word64,
    scpCurrentOffered :: !Double
  }
  deriving (Show, Eq)

-- https://github.com/lexi-lambda/mtl-style-example/blob/master/library/MTLStyleExample/Interfaces.hs

simulateChargingPoint :: Session -> InputEvent SetChargingProfile -> (OutputEvent [] MeterValues, ())
simulateChargingPoint (Session (SessionConfiguration {connectorId, stationId, getInstantaneousCurrent}) sessionState) (InputEvent {ieTick}) = do
  case sessionState of
    Charging {transactionId, energyDelivered, currentOffered} -> do
      tellNextSimulationStepAndEvent (minutes 2 <> seconds 14) $
        MeterValues
          { mvTransactionId = transactionId,
            mvStationId = stationId,
            mvConnectorId = connectorId,
            mvTimestamp = ieTick,
            mvCurrents = getInstantaneousCurrent ieTick sessionState,
            mvOfferedCurrent = currentOffered,
            mvEnergyDelivered = energyDelivered
          }
      return ()
    _ -> tellNextSimulationStep $ minutes 1

stepSessionState :: Session -> Duration -> InputEvent SetChargingProfile -> (OutputEvent [] MeterValues, SessionState)
stepSessionState (Session (SessionConfiguration {charge, getInstantaneousCurrent, meterValuesPeriodicity, stationId, connectorId}) sessionState) stepSize (InputEvent {ieTick = now}) = do
  sessionState <- charge sessionState stepSize now
  let sampleDelay = milliseconds 100 -- delay between taking the MeterValues sample and making the output event
      shortestDelayUntilNextSample = milliseconds 1 -- how long to wait before making new MeterValues sample if we are late
      updateMeterValuesStateMachine :: SessionState -> (OutputEvent [] MeterValues, SessionState)
      updateMeterValuesStateMachine newState@(Charging {transactionId, currentOffered, energyDelivered, meterValuesStateMachine = (NextMeterValueSampleDue sampleTs)})
        | now < sampleTs = return newState
        | otherwise = do
            tellNextSimulationStep sampleDelay
            return $
              newState
                { meterValuesStateMachine =
                    Sampled
                      now
                      MeterValues
                        { mvTransactionId = transactionId,
                          mvStationId = stationId,
                          mvConnectorId = connectorId,
                          mvTimestamp = now,
                          mvCurrents = getInstantaneousCurrent now newState,
                          mvOfferedCurrent = currentOffered,
                          mvEnergyDelivered = energyDelivered
                        }
                }
      updateMeterValuesStateMachine newState@(Charging {transactionId, currentOffered, energyDelivered, meterValuesStateMachine = sampled@(Sampled sampleTs sampledValue)})
        | now >= (sampleDelay `after` sampleTs) = do
            let idealNextSample = meterValuesPeriodicity `after` sampleTs
            let (nextSampleDue, delayToNextSample) =
                  if now < idealNextSample
                    then (idealNextSample, now `durationUntil` idealNextSample)
                    else (shortestDelayUntilNextSample `after` now, shortestDelayUntilNextSample)
            tellNextSimulationStepAndEvent delayToNextSample sampledValue
            return $ newState {meterValuesStateMachine = NextMeterValueSampleDue nextSampleDue}
        | otherwise = do
            tellNextSimulationStep $ now `durationUntil` (sampleDelay `after` sampleTs)
            return newState
      updateMeterValuesStateMachine newState = return newState
   in updateMeterValuesStateMachine sessionState

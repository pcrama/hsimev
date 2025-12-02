module ChargingStation
  ( stepSimulation,
    SessionTarget (..),
    Timestamp,
    Duration,
    MeterValues (..),
    MeterValuesStateMachine (..),
    Phases (..),
    SessionState (..),
    SessionConfiguration (..),
    Session (..),
    SimState (..),
    chargeEfficientlyUntil80Percent,
    instantaneousCurrentMaxUntil80Percent,
  )
where

import Data.Text (Text)
import Data.Word (Word64, Word8)

-- | [ms] since some imaginary epoch
type Timestamp = Word64

type Duration =
  -- | [ms]
  Word64

data SessionTarget
  = -- | driver wants to leave at a fixed time
    LeaveAtTick !Timestamp
  | -- | driver wants to leave at the earliest after the given time but only if their charge level is sufficient
    LeaveAfterBoth !Timestamp !Double
  | -- | driver will stay until the given time or as soon as their battery has reached that charge level
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
  | -- | Two phases
    RS
  | -- | Two phases
    ST
  | -- | Two phases
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
  | SuspendedEV
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
    charge :: Timestamp -> SessionState -> Duration -> SessionState,
    instantaneousCurrent :: Timestamp -> SessionState -> (Double, Double, Double),
    phases :: !Phases,
    stationId :: !Text,
    connectorId :: !Word8,
    meterValuesPeriodicity :: !Duration
  }

data Session = Session !SessionConfiguration !SessionState

data SimState = SimState
  { tick :: !Timestamp,
    session :: !Session
  }

countPhases :: (Double, Double, Double) -> Int
countPhases (x, y, z) = (if x > 0.0 then 1 else 0) + (if y > 0.0 then 1 else 0) + (if z > 0.0 then 1 else 0)

chargeEfficientlyUntil80Percent ::
  SessionConfiguration ->
  Timestamp ->
  SessionState ->
  Duration ->
  SessionState
chargeEfficientlyUntil80Percent _ _ Available _ = Available
chargeEfficientlyUntil80Percent _ _ Preparing _ = Preparing
chargeEfficientlyUntil80Percent
  (SessionConfiguration {instantaneousCurrent, sessionTarget, batteryCapacity})
  t0
  sessionState@(Charging {energyDelivered, batteryLevel, currentOffered})
  dt =
    case sessionTarget of
      LeaveAtTick ts | ts <= t0 + dt -> Available
      LeaveAfterBoth ts targetLevel | (ts <= t0 + dt) && (targetLevel <= newLevel) -> Available
      LeaveAfterEither ts targetLevel | (ts <= t0 + dt) || (targetLevel <= newLevel) -> Available
      LeaveAtLevel targetLevel | targetLevel <= newLevel -> Available
      _ -> sessionState {energyDelivered = newEnergyDelivered, batteryLevel = newLevel}
    where
      voltage = 230
      sumOfCurrents = let (x, y, z) = instantaneousCurrent t0 sessionState in x + y + z
      deltaEnergyDelivered = (voltage * sumOfCurrents * fromIntegral dt) / 3600.0 / 1000.0
      batteryPercentage = batteryLevel / batteryCapacity
      chargingEfficiency = if batteryPercentage < 0.8 then 0.85 else 0.75
      newLevel = batteryLevel + deltaEnergyDelivered * chargingEfficiency
      newEnergyDelivered = energyDelivered + deltaEnergyDelivered

instantaneousCurrentMaxUntil80Percent ::
  Double ->
  Double ->
  SessionConfiguration ->
  -- | timestamp, ignored
  Timestamp ->
  SessionState ->
  (Double, Double, Double)
instantaneousCurrentMaxUntil80Percent _ _ _ _ Available = (0.0, 0.0, 0.0)
instantaneousCurrentMaxUntil80Percent _ _ _ _ Preparing = (0.0, 0.0, 0.0)
instantaneousCurrentMaxUntil80Percent
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

stepSessionState ::
  -- | tick of the SimState, associated with current SessionState value
  Timestamp ->
  -- | configuration of the session
  SessionConfiguration ->
  -- | session's state at t0
  SessionState ->
  -- | timestamp for which to compute new session state
  Timestamp ->
  SessionState
stepSessionState t0 sessionConfiguration@(SessionConfiguration {instantaneousCurrent, meterValuesPeriodicity, stationId, connectorId}) sessionState ts
  | ts > t0 = updateMeterValuesStateMachine $ charge sessionConfiguration t0 sessionState (ts - t0)
  where
    updateMeterValuesStateMachine :: SessionState -> SessionState
    updateMeterValuesStateMachine newState@(Charging {transactionId, currentOffered, energyDelivered, meterValuesStateMachine = (NextMeterValueSampleDue sampleTs)})
      | ts < sampleTs = newState
      | otherwise =
          newState
            { meterValuesStateMachine =
                Sampled
                  ts
                  MeterValues
                    { mvTransactionId = transactionId,
                      mvStationId = stationId,
                      mvConnectorId = connectorId,
                      mvTimestamp = ts,
                      mvCurrents = instantaneousCurrent ts newState,
                      mvOfferedCurrent = currentOffered,
                      mvEnergyDelivered = energyDelivered
                    }
            }
    updateMeterValuesStateMachine newState@(Charging {transactionId, currentOffered, energyDelivered, meterValuesStateMachine = (Sampled sampleTs _)})
      -- TODO: send those meter values somewhere!
      | ts > (sampleTs + 100) = newState {meterValuesStateMachine = NextMeterValueSampleDue $ max (ts + 1) (sampleTs + meterValuesPeriodicity)}
      | otherwise =
          newState
            { meterValuesStateMachine =
                Sampled
                  ts
                  MeterValues
                    { mvTransactionId = transactionId,
                      mvStationId = "which station?",
                      mvConnectorId = 255,
                      mvTimestamp = ts,
                      mvCurrents = instantaneousCurrent ts newState,
                      mvOfferedCurrent = currentOffered,
                      mvEnergyDelivered = energyDelivered
                    }
            }
stepSessionState _ _ _ _ = error "Can't simulate time backwards"

stepSimulation :: SimState -> Timestamp -> SimState
stepSimulation (SimState {tick = t0}) ts
  | ts <= 0 = error "Can't simulate time backwards"
stepSimulation
  simState@(SimState {tick = t0, session = (Session sessionConfiguration sessionState@(Charging {}))})
  ts = simState {tick = ts, session = Session sessionConfiguration $ stepSessionState t0 sessionConfiguration sessionState ts}
stepSimulation simState ts = simState {tick = ts}

data InputEvent a = InputEvent
  { ieTick :: !Timestamp,
    ieTrigger :: !(Maybe a)
  }
  deriving (Show, Eq)

data OutputEvent b = OutputEvent
  { -- | Request for next step no later than after this duration
    oeNext :: !Duration,
    oeEvent :: !(Maybe b)
  }
  deriving (Show, Eq)


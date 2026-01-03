{-# LANGUAGE Safe #-}

module ChargingStation
  ( Duration,
    InputEvent (..),
    MeterValues (..),
    MeterValuesStateMachine (..),
    OutputEvent (..),
    Phases (..),
    Session (..),
    SessionConfiguration (..),
    SessionOutput (..),
    SessionState (..),
    SessionTarget (..),
    SimulationSetChargingProfile (..),
    SimState (..),
    Timestamp (..),
    TransactionId (..),
    after,
    before,
    clampingDurationUntil,
    durationUntil,
    maybeDurationUntil,
    milliseconds,
    minutes,
    seconds,
    secondsFrom,
    simulationTimeToUTCTime,
    utcTimeToSimulationTime,
    chargeEfficientlyUntil80Percent,
    getInstantaneousCurrentMaxUntil80Percent,
    simulate,
    stepSession,
    stepSimulation,
  )
where

import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Functor (void)
import Data.Kind (Constraint, Type)
import Data.Monoid (First (..), Last (..))
import Data.Text (Text)
import Data.Time.Clock
  ( UTCTime (..),
    addUTCTime,
    diffUTCTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import Data.Word (Word64, Word8)
import Prelude

-- | [ms] since some imaginary epoch
type Timestamp :: Type
newtype Timestamp = Timestamp Word64
  deriving stock (Show, Eq, Ord)

type Duration :: Type
newtype Duration
  = Duration
      -- | [ms]
      Word64
  deriving stock (Show, Eq, Ord)

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

maybeDurationUntil :: (MonadFail m) => Timestamp -> Timestamp -> m Duration
maybeDurationUntil (Timestamp from_) (Timestamp to_)
  | from_ <= to_ = return $ Duration $ to_ - from_
  | otherwise = fail "Negative duration not allowed"

secondsFrom :: Duration -> Double
secondsFrom (Duration dur) = fromIntegral dur / 1000.0

minutes :: Word64 -> Duration
minutes = Duration . (* 60000)

seconds :: Word64 -> Duration
seconds = Duration . (* 1000)

milliseconds :: Word64 -> Duration
milliseconds = Duration

simulationTimeToUTCTime :: Timestamp -> UTCTime -> Timestamp -> UTCTime
simulationTimeToUTCTime t0 refTime t =
  case t0 `maybeDurationUntil` t of
    Just duration -> secondsToNominalDiffTime (fromRational (toRational $ secondsFrom duration)) `addUTCTime` refTime
    Nothing -> secondsToNominalDiffTime ((-1) * fromRational (toRational (secondsFrom $ t `durationUntil` t0))) `addUTCTime` refTime

utcTimeToSimulationTime :: Timestamp -> UTCTime -> UTCTime -> Timestamp
utcTimeToSimulationTime t0 refTime u
  | refTime >= u = milliseconds (round $ 1000 * nominalDiffTimeToSeconds (refTime `diffUTCTime` u)) `before` t0
  | otherwise = milliseconds (round $ 1000 * nominalDiffTimeToSeconds (u `diffUTCTime` refTime)) `after` t0

type SessionTarget :: Type
data SessionTarget
  = -- | driver wants to leave at a fixed time
    LeaveAtTick !Timestamp
  | -- | driver wants to leave at the earliest after the given time but only if their charge level in [Wh] is sufficient
    LeaveAfterBoth !Timestamp !Double
  | -- | driver will stay until the given time or leave as soon as their battery has reached that charge level in [Wh]
    LeaveAfterEither !Timestamp !Double
  | -- | drive will stay until their battery has reached that charge level in [Wh]
    LeaveAtLevel !Double
  deriving stock (Show, Eq)

type Phases :: Type
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
  deriving stock (Show, Eq)

type SessionState :: Type
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
        transactionId :: !TransactionId,
        startDateTime :: !Timestamp,
        meterValuesStateMachine :: !MeterValuesStateMachine
      }
  | Finishing
  deriving stock (Show, Eq)

type MeterValuesStateMachine :: Type
data MeterValuesStateMachine
  = -- | Meter values were sampled and sent, we're waiting for the next time to sample new values
    NextMeterValueSampleDue !Timestamp
  | -- | Meter values are sampled, we're waiting for the right moment to send them
    Sampled !Timestamp !MeterValues
  deriving stock (Show, Eq)

type TransactionId :: Type
newtype TransactionId = TransactionId Text
  deriving stock (Show, Eq, Ord)

type MeterValues :: Type
data MeterValues = MeterValues
  { mvTransactionId :: !TransactionId,
    mvStationId :: !Text,
    mvConnectorId :: !Word8,
    mvStartDateTime :: !Timestamp,
    mvTimestamp :: !Timestamp,
    mvCurrents :: !(Double, Double, Double),
    mvOfferedCurrent :: !Double,
    mvEnergyDelivered :: !Double
  }
  deriving stock (Show, Eq)

type SessionOutput :: Type
data SessionOutput
  = StartCharge !Timestamp !TransactionId !Text !Word8
  | SendMeterValues !MeterValues
  | AcceptSetChargingProfile !Text
  | RejectSetChargingProfile !Text
  | TimeoutSetChargingProfile !Text
  | EndOfCharge !MeterValues
  deriving stock (Show, Eq)

type SessionConfiguration :: Type
data SessionConfiguration = SessionConfiguration
  { -- | how many [Wh] "fit" in the car's battery
    batteryCapacity :: !Double,
    sessionTarget :: !SessionTarget,
    -- | update session state to account for passing of time
    charge :: SessionState -> Duration -> Timestamp -> (OutputEvent [] SessionOutput, SessionState),
    getInstantaneousCurrent :: Timestamp -> SessionState -> (Double, Double, Double),
    phases :: !Phases,
    stationId :: !Text,
    connectorId :: !Word8,
    meterValuesPeriodicity :: !Duration
  }

type Session :: Type
data Session = Session !SessionConfiguration !SessionState

instance Show Session where
  show (Session (SessionConfiguration {batteryCapacity, sessionTarget, phases, stationId, connectorId, meterValuesPeriodicity}) sessionState) = "Session (SessionConfiguration {batteryCapacity=" <> show batteryCapacity <> ", sessionTarget=" <> show sessionTarget <> ", phases=" <> show phases <> ", stationId=" <> show stationId <> ", connectorId=" <> show connectorId <> ", meterValuesPeriodicity=" <> show meterValuesPeriodicity <> "}) (" <> show sessionState <> ")"

type SimState :: Type
data SimState = SimState {tick :: !Timestamp, session :: !Session}
  deriving stock (Show)

type InputEvent :: Type -> Type
data InputEvent a = InputEvent
  { ieTick :: !Timestamp,
    ieTrigger :: !(Maybe a)
  }
  deriving stock (Show, Eq)

type OutputEvent :: (Type -> Type) -> Type -> Type
data OutputEvent w b
  = NoEvent
  | OutputEvent
      { -- | Request for next step no later than after this duration
        oeNext :: !Duration,
        -- | Produce those output events `b` right now and next step no later than after this duration
        oeEvent :: !(w b)
      }
  deriving stock (Show, Eq)

instance (Semigroup (w b)) => Semigroup (OutputEvent w b) where
  lf <> NoEvent = lf
  NoEvent <> rg = rg
  (OutputEvent {oeNext = lfNxt, oeEvent = lfEvts}) <> (OutputEvent {oeNext = rgNxt, oeEvent = rgEvts}) =
    OutputEvent
      { oeNext = min lfNxt rgNxt,
        oeEvent = lfEvts <> rgEvts
      }

instance (Semigroup (w b)) => Monoid (OutputEvent w b) where
  mempty = NoEvent

type SemigroupWrapper :: (Type -> Type) -> Type -> Constraint
class (Semigroup (w b)) => SemigroupWrapper w b where
  wrapInSemigroup :: b -> w b

instance SemigroupWrapper First b where
  wrapInSemigroup = First . Just

instance SemigroupWrapper Last b where
  wrapInSemigroup = Last . Just

instance SemigroupWrapper [] b where
  wrapInSemigroup = (: [])

tellNextSimulationStep :: (Monoid (w b), MonadWriter (OutputEvent w b) m) => Duration -> m ()
tellNextSimulationStep dur = tell $ OutputEvent {oeNext = dur, oeEvent = mempty}

tellNextSimulationStepAndEvent :: (SemigroupWrapper w b, MonadWriter (OutputEvent w b) m) => Duration -> b -> m b
tellNextSimulationStepAndEvent dur evt = do
  tell $ OutputEvent {oeNext = dur, oeEvent = wrapInSemigroup evt}
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
  (OutputEvent [] SessionOutput, SessionState)
chargeEfficientlyUntil80Percent _ Available _ _ = return Available
chargeEfficientlyUntil80Percent _ Preparing _ _ = return Preparing
chargeEfficientlyUntil80Percent
  (SessionConfiguration {getInstantaneousCurrent, sessionTarget, batteryCapacity, stationId, connectorId})
  sessionState@(Charging {energyDelivered, batteryLevel, currentOffered, transactionId, startDateTime})
  dt
  nextTime = do
    case sessionTarget of
      LeaveAtTick ts
        | ts <= nextTime -> endOfCharge
        | otherwise -> accumulateEnergy $ nextTime `durationUntil` ts
      LeaveAfterBoth ts targetLevel
        | (ts <= nextTime) && (targetLevel <= newLevel) -> endOfCharge
        | ts <= nextTime -> accumulateEnergy defaultNextSimulationStep
        | otherwise -> accumulateEnergy $ nextTime `durationUntil` ts
      LeaveAfterEither ts targetLevel
        | (ts <= nextTime) || (targetLevel <= newLevel) -> endOfCharge
        | otherwise -> accumulateEnergy defaultNextSimulationStep
      LeaveAtLevel targetLevel
        | targetLevel <= newLevel -> endOfCharge
        | otherwise -> accumulateEnergy defaultNextSimulationStep
    where
      instantaneousCurrent = getInstantaneousCurrent (dt `before` nextTime) sessionState
      defaultNextSimulationStep = minutes 5
      voltage = 230
      sumOfCurrents = let (x, y, z) = instantaneousCurrent in x + y + z
      deltaEnergyDelivered = (voltage * sumOfCurrents * secondsFrom dt) / 3600.0 -- 3600 to convert from [J] to [Wh]
      batteryPercentage = batteryLevel / batteryCapacity
      chargingEfficiency = if batteryPercentage < 0.8 then 0.85 else 0.75
      newLevel = min batteryCapacity $ batteryLevel + deltaEnergyDelivered * chargingEfficiency
      newEnergyDelivered = energyDelivered + (newLevel - batteryLevel) / max 0.1 chargingEfficiency
      currentMeterValues =
        MeterValues
          { mvTransactionId = transactionId,
            mvStationId = stationId,
            mvConnectorId = connectorId,
            mvStartDateTime = startDateTime,
            mvTimestamp = nextTime,
            mvCurrents = (0, 0, 0),
            mvOfferedCurrent = currentOffered,
            mvEnergyDelivered = newEnergyDelivered
          }
      accumulateEnergy nextStep = do
        tellNextSimulationStep nextStep
        return sessionState {energyDelivered = newEnergyDelivered, batteryLevel = newLevel}
      endOfCharge = do
        void $ tellNextSimulationStepAndEvent defaultNextSimulationStep $ EndOfCharge currentMeterValues
        return Available

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

stepSimulation :: SimState -> Timestamp -> (OutputEvent [] SessionOutput, SimState)
stepSimulation (SimState {tick = t0}) ts
  | ts <= t0 = error "Can't simulate time backwards"
stepSimulation
  simState@(SimState {tick = t0, session = session@(Session sessionConfiguration (Charging {}))})
  ts = do
    newState <- stepSessionState session (t0 `durationUntil` ts) $ InputEvent {ieTick = ts, ieTrigger = Nothing}
    return simState {tick = ts, session = Session sessionConfiguration newState}
stepSimulation simState ts = return simState {tick = ts}

-- | OCPI 2.2.1's SetChargingProfile is parsed to a
-- SimulationSetChargingProfile if the SetChargingProfile conforms to a
-- simplistic format that represents a constant setpoint until the end of the
-- charge or the next SetChargingProfile overriding it.
type SimulationSetChargingProfile :: Type
data SimulationSetChargingProfile = SimulationSetChargingProfile
  { -- | transactionId for which the setpoint should be applied
    scpTransactionId :: !TransactionId,
    scpCurrentOffered :: !Double,
    scpCallback :: !Text
  }
  deriving stock (Show, Eq)

-- https://github.com/lexi-lambda/mtl-style-example/blob/master/library/MTLStyleExample/Interfaces.hs

stepSessionState :: Session -> Duration -> InputEvent SimulationSetChargingProfile -> (OutputEvent [] SessionOutput, SessionState)
stepSessionState session@(Session (SessionConfiguration {charge, getInstantaneousCurrent, meterValuesPeriodicity, stationId, connectorId}) _) stepSize (InputEvent {ieTick = now, ieTrigger}) =
  extractSessionState session
    >>= (\s -> charge s stepSize now)
    >>= updateOfferedCurrent ieTrigger
    >>= updateMeterValuesStateMachine
  where
    defaultNextSimulationStep = minutes 5
    -- Avoid naming the session state because it will be updated along a
    -- "chain", so we make sure that no element of the chain can accidentally
    -- refer to the value several "links" up the chain.
    extractSessionState (Session _ state) = return state

    updateOfferedCurrent :: Maybe SimulationSetChargingProfile -> SessionState -> (OutputEvent [] SessionOutput, SessionState)
    updateOfferedCurrent Nothing sessStt = return sessStt
    updateOfferedCurrent (Just SimulationSetChargingProfile{scpTransactionId, scpCurrentOffered, scpCallback}) sessStt@(Charging {transactionId})
      | scpTransactionId == transactionId = do
          -- TODO: check if offered current is within acceptable range and return RejectSetChargingProfile if needed
         void $ tellNextSimulationStepAndEvent defaultNextSimulationStep $ AcceptSetChargingProfile scpCallback
         return $ sessStt {currentOffered = scpCurrentOffered}
      | otherwise = return sessStt
    updateOfferedCurrent (Just _) sessStt = return sessStt

    shortestDelayUntilNextSample = milliseconds 1 -- how long to wait before making new MeterValues sample if we are late
    sampleDelay = milliseconds 100 -- delay between taking the MeterValues sample and making the output event
    updateMeterValuesStateMachine :: SessionState -> (OutputEvent [] SessionOutput, SessionState)
    updateMeterValuesStateMachine newState@(Charging {transactionId, startDateTime, currentOffered, energyDelivered, meterValuesStateMachine = (NextMeterValueSampleDue sampleTs)})
      | now < sampleTs = do
          tellNextSimulationStep $ now `durationUntil` sampleTs
          return newState
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
                        mvStartDateTime = startDateTime,
                        mvTimestamp = now,
                        mvCurrents = getInstantaneousCurrent now newState,
                        mvOfferedCurrent = currentOffered,
                        mvEnergyDelivered = energyDelivered
                      }
              }
    updateMeterValuesStateMachine newState@(Charging {meterValuesStateMachine = (Sampled sampleTs sampledValue)})
      | now >= (sampleDelay `after` sampleTs) = do
          let idealNextSample = meterValuesPeriodicity `after` sampleTs
          let (nextSampleDue, delayToNextSample) =
                case now `maybeDurationUntil` idealNextSample of
                  Just duration -> (idealNextSample, duration)
                  Nothing -> (shortestDelayUntilNextSample `after` now, shortestDelayUntilNextSample)
          void $ tellNextSimulationStepAndEvent delayToNextSample $ SendMeterValues sampledValue
          return $ newState {meterValuesStateMachine = NextMeterValueSampleDue nextSampleDue}
      | otherwise = do
          tellNextSimulationStep $ now `durationUntil` (sampleDelay `after` sampleTs)
          return newState
    updateMeterValuesStateMachine newState = return newState

-- | Step 'Session' (i.e. update its state from ('InputEvent'.'ieTick' - 'Duration') until 'ieTick')
stepSession :: Session -> Duration -> InputEvent SimulationSetChargingProfile -> (OutputEvent [] SessionOutput, Session)
stepSession session@(Session sessionConfiguration _) duration inputEvent = (outputEvent, Session sessionConfiguration nextState)
  where
    (outputEvent, nextState) = stepSessionState session duration inputEvent

simulate ::
  (Monad m) =>
  -- | Update 'Session'\'s state (assumed to be at 'InputEvent'.'ieTick' - 'Duration') to become the state
  -- at 'InputEvent'.'ieTick'.
  (Session -> Duration -> InputEvent i -> (OutputEvent [] e, Session)) ->
  -- | Monadic action delivering the output events that were generated while "stepping" the 'Session'
  (e -> m ()) ->
  -- | Continue the simulation from the given state, where the next step is
  -- given by the 'InputEvent' (unless an external event source provides an
  -- event first)
  (SimState -> InputEvent i -> m ()) ->
  SimState ->
  InputEvent i ->
  m ()
simulate
  stpSess
  deliverEvent
  recurse
  (SimState {tick, session})
  inputEvent@(InputEvent {ieTick = nextTick}) = do
    case stpSess session (tick `durationUntil` nextTick) inputEvent of
      (NoEvent, _) -> return ()
      (OutputEvent {oeNext, oeEvent}, nextSession) -> do
        forM_ oeEvent deliverEvent
        recurse (SimState {tick = nextTick, session = nextSession}) $ InputEvent {ieTick = oeNext `after` nextTick, ieTrigger = Nothing}

{-# LANGUAGE Trustworthy #-}

module OcpiStation
  ( Config (..),
    MultiSession,
    SessionKey,
    deliverSessionOutputIO,
    parseSetChargingProfile,
    startMultiSimulation,
    startSimulation,
  )
where

import ChargingStation
import Control.Concurrent (MVar, takeMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (tell)
import Data.Functor (void)
import Data.Kind (Type)
import Data.List (find)
import Data.Semigroup (Min (..))
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime (..))
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word8)
import Debug.Trace (trace)
import Ocpi221 qualified as O
import PriorityMap qualified
import System.Timeout qualified as SysTimeout
import Prelude

type Config :: Type
data Config = Config {scspBaseUrl :: !Text, scspToken :: !String, timestampToUtcTime :: !(Timestamp -> UTCTime)}

instance Show Config where
  show (Config {scspBaseUrl, scspToken, timestampToUtcTime}) =
    "Config {scspBaseUrl="
      <> show scspBaseUrl
      <> ", scspToken="
      <> show scspToken
      <> ", timestampToUtcTime=<t0:"
      <> unpack (encodeSimulationTimestampForOcpi timestampToUtcTime $ Timestamp 0)
      <> ">}"

encodeSimulationTimestampForOcpi :: (Timestamp -> UTCTime) -> Timestamp -> Text
encodeSimulationTimestampForOcpi shiftTime = O.encodeTimeAsRFC3339 . shiftTime

deliverSessionOutputIO :: (MonadIO m) => Config -> SessionOutput -> m ()
deliverSessionOutputIO config so = case so of
  StartCharge timestamp transactionId stationId connectorId -> putSession $ makeStartSession timestamp transactionId stationId connectorId
  SendMeterValues meterValues -> putSession $ makeMeterValuesSession meterValues
  AcceptSetChargingProfile cbUrl -> postCallback cbUrl O.CprtAccepted
  RejectSetChargingProfile cbUrl -> postCallback cbUrl O.CprtRejected
  TimeoutSetChargingProfile cbUrl -> postCallback cbUrl O.CprtUnknown -- TODO
  EndOfCharge meterValues ->
    let session = makeMeterValuesSession meterValues
     in putSession $
          session
            { O.ocpi_session_status = "COMPLETED",
              O.ocpi_session_end_date_time = pure $ O.ocpi_session_last_updated session
            }
  where
    putSession = void . O.putSessionRequestIO (scspBaseUrl config) (scspToken config)
    postCallback cpUrl cprt = void $ O.postCallbackRequestIO (scspToken config) cpUrl cprt
    encodeTimestamp = encodeSimulationTimestampForOcpi (timestampToUtcTime config)
    makeStartSession timestamp (TransactionId transactionId) stationId connectorId =
      O.Session
        { O.ocpi_session_country_code = "NL",
          O.ocpi_session_party_id = "LMS",
          O.ocpi_session_id = transactionId,
          O.ocpi_session_start_date_time = encodeTimestamp timestamp,
          O.ocpi_session_end_date_time = Nothing,
          O.ocpi_session_kwh = 0,
          O.ocpi_session_cdr_token =
            O.CdrToken
              { O.ocpi_cdr_country_code = "NL",
                O.ocpi_cdr_party_id = "LMS",
                O.ocpi_cdr_uid = "cdr_UID",
                O.ocpi_cdr_token_type = "RFID",
                O.ocpi_cdr_contract_id = "contract_id"
              },
          O.ocpi_session_auth_method = "COMMAND",
          O.ocpi_session_location_id = "loc_id",
          O.ocpi_session_evse_uid = stationId,
          O.ocpi_session_connector_id = pack $ show connectorId,
          O.ocpi_session_currency = "EUR",
          O.ocpi_session_status = "ACTIVE",
          O.ocpi_session_last_updated = encodeTimestamp timestamp
        }
    makeMeterValuesSession
      ( MeterValues
          { mvTransactionId = TransactionId transactionId,
            mvStationId,
            mvConnectorId,
            mvStartDateTime,
            mvTimestamp,
            mvCurrents,
            mvOfferedCurrent,
            mvEnergyDelivered
          }
        ) =
        O.Session
          { O.ocpi_session_country_code = "NL",
            O.ocpi_session_party_id = "LMS",
            O.ocpi_session_id = transactionId,
            O.ocpi_session_start_date_time = encodeTimestamp mvStartDateTime,
            O.ocpi_session_end_date_time = Nothing,
            O.ocpi_session_kwh = O.fromWhTo_kWh mvEnergyDelivered,
            O.ocpi_session_cdr_token =
              O.CdrToken
                { O.ocpi_cdr_country_code = "NL",
                  O.ocpi_cdr_party_id = "LMS",
                  O.ocpi_cdr_uid = "cdr_ID",
                  O.ocpi_cdr_token_type = "RFID",
                  O.ocpi_cdr_contract_id = "contract_id"
                },
            O.ocpi_session_auth_method = "COMMAND",
            O.ocpi_session_location_id = "loc_id",
            O.ocpi_session_evse_uid = mvStationId,
            O.ocpi_session_connector_id = pack $ show mvConnectorId,
            O.ocpi_session_currency = "EUR",
            O.ocpi_session_status = "ACTIVE",
            O.ocpi_session_last_updated = encodeTimestamp mvTimestamp
          }

parseSetChargingProfile :: TransactionId -> O.SetChargingProfile -> Maybe SimulationSetChargingProfile
parseSetChargingProfile
  transactionId
  ( O.SetChargingProfile
      { O.response_url,
        O.charging_profile =
          O.ChargingProfile
            { O.charging_rate_unit = "A",
              O.charging_profile_period = [O.ChargingProfilePeriod {O.limit}]
            }
      }
    ) =
    Just $
      SimulationSetChargingProfile
        { scpTransactionId = transactionId,
          scpCurrentOffered = limit,
          scpCallback = response_url
        }
parseSetChargingProfile _ _ = Nothing

getNextEvent ::
  (MonadIO m, Show i) =>
  -- | conversion function from wall clock time to simulation time
  (UTCTime -> Timestamp) ->
  -- | Channel to query for next event from external source
  MVar i ->
  -- | External event may not be later than this timestamp (expressed in
  -- simulation time)
  Timestamp ->
  -- | How long to wait at most for an external event (this value is related
  -- to the deadline: duration `after` (wallClockToTimestamp getCurrentTime) ≅
  -- deadline).
  Duration ->
  -- | Default event to return if no event comes from the external source in time.
  InputEvent i ->
  m (InputEvent i)
getNextEvent wallClockToTimestamp ch deadline maxWaitTime defaultEvent = do
  liftIO $ putStrLn $ "getNextEvent: deadline=" <> show deadline <> ", maxWaitTime=" <> show maxWaitTime <> ", defaultEvent=" <> show defaultEvent
  mbEvt <- liftIO $ SysTimeout.timeout (round $ 1000000 * secondsFrom maxWaitTime) $ takeMVar ch
  case mbEvt of
    Just evt -> do
      now <- wallClockToTimestamp <$> liftIO getCurrentTime
      -- When waiting for an external event, avoid the situation where the
      -- external event falls just after (because of the processing time) or
      -- exactly at the same time as evt's ieTrigger and evt's ieTrigger gets
      -- lost.
      return $ InputEvent {ieTick = min now deadline, ieTrigger = Just evt}
    Nothing -> return defaultEvent

simulator :: (MonadIO m) => Config -> (SimState -> InputEvent SimulationSetChargingProfile -> m ()) -> SimState -> InputEvent SimulationSetChargingProfile -> m ()
simulator config = simulate stepSession (deliverSessionOutputIO config)

startSimulation :: (MonadIO m) => Config -> MVar SimulationSetChargingProfile -> Session -> Timestamp -> Duration -> m ()
startSimulation config eventChannel session t0 duration = do
  refTime <- liftIO getCurrentTime -- refTime [wall clock time] corresponds to t0 [simulation time]
  let wallClockToTimestamp = utcTimeToSimulationTime t0 refTime
  simulator
    config
    (recurseSimulation config wallClockToTimestamp (duration `after` t0) eventChannel)
    (SimState {tick = t0, session = session})
    (InputEvent {ieTick = milliseconds 1 `after` t0, ieTrigger = Nothing})

recurseSimulation ::
  (MonadIO m) =>
  Config ->
  -- | Conversion function from current (wall clock time) to simulation timestamps
  (UTCTime -> Timestamp) ->
  -- | When is simulation finished?
  Timestamp ->
  -- | Channel to query for next event from external source
  MVar SimulationSetChargingProfile ->
  SimState ->
  InputEvent SimulationSetChargingProfile ->
  m ()
recurseSimulation config wallClockToTimestamp finalTimestamp eventChannel simState evt@(InputEvent {ieTick, ieTrigger}) = do
  now <- wallClockToTimestamp <$> liftIO getCurrentTime
  -- When waiting for an external event, avoid the situation where the
  -- external event falls exactly at the same time as evt's ieTrigger and
  -- evt's ieTrigger gets lost.
  let externalEventDeadline = case ieTrigger of
        Just _ -> milliseconds 1 `before` ieTick
        Nothing -> ieTick
  liftIO $ putStrLn $ "now=" <> show now <> ", externalEventDeadline=" <> show externalEventDeadline <> ", simState=" <> show simState <> ", evt=" <> show evt
  let continueSimulation = simulator config (recurseSimulation config wallClockToTimestamp finalTimestamp eventChannel) simState
  if now < finalTimestamp
    then case now `maybeDurationUntil` min externalEventDeadline finalTimestamp of
      Just duration -> getNextEvent wallClockToTimestamp eventChannel (duration `after` now) duration evt >>= continueSimulation
      Nothing -> continueSimulation evt
    else liftIO $ putStrLn $ "Simulation done at " <> show now

-- | In the case of simulating multiple charging sessions in parallel, when an
-- OCPI input event (SetChargingProfile) arrives, it must be dispatched to the
-- relevant charging session.  'SessionKey' is the type that allows to make
-- this link: by observing the events delivered to the outside world and
-- comparing them to the events coming in from the outside world.
type SessionKey :: Type
type SessionKey = (Text, Word8)

-- | A group of 'Session's simulated in parallel
--
-- Represented as a 'PriorityMap.PriorityMap' with
-- - Priority: Next timestamp at which the session wants to be woken up at the latest to simulated
-- - Key: ('O.ocpi_session_evse_uid', 'O.ocpi_session_connector_id') to route
--   external input events to the proper 'Session'
-- - Value: A 'Session' and a 'Timestamp' up to which it was simulated
type MultiSession :: Type
type MultiSession = PriorityMap.PriorityMap Timestamp SessionKey (Session, Timestamp)

type MultiSimulator :: Type
newtype MultiSimulator = MultiSimulator MultiSession
  deriving stock (Show)

-- | Step all 'Session's from the 'MultiSession' that needed to be updated before the 'InputEvent'\'s 'ieTick'
--
-- * Case 1: InputEvent {ieTick=m, ieTrigger=Nothing}
--
-- Go through all individual 'Session's that wanted to run at or before `m`
-- (as evidenced by their priority) and update their state to reach `m`.
--
-- * Case 2: InputEvent {ieTick=m
--                      , ieTrigger=Just (("station_id", 1), SimulationSetChargingProfile {scpTransactionId="CSMS0123", ...})}
--
-- - Step 'Session' with key=("station_id", 1) up to 'm' with the
--   'SimulationSetChargingProfile' we received
-- - Go through all individual 'Session's that wanted to run at or before `m`
--   (as evidenced by their priority) and update their state to reach 'm'.
multiStepper :: MultiSession -> InputEvent (SessionKey, SimulationSetChargingProfile) -> (OutputEvent [] SessionOutput, MultiSession)
multiStepper allSessions (InputEvent {ieTick, ieTrigger}) =
  case ieTrigger of
    Nothing -> multiStepperNoTrigger allSessions ieTick
    Just trigger -> multiStepperForTrigger allSessions ieTick trigger

-- | Internal function for multiStepper, factored out for clarity
multiStepperNoTrigger :: MultiSession -> Timestamp -> (OutputEvent [] SessionOutput, MultiSession)
multiStepperNoTrigger allSessions tick = do
  case PriorityMap.splitAfterFirst allSessions of
    Nothing -> return allSessions
    Just ((oldPrio, sessKey, (sess, oldSessTimestamp)), tailSessions)
      | oldPrio > tick -> return allSessions
      | otherwise ->
          case oldSessTimestamp `maybeDurationUntil` tick of
            Just d -> do
              case stepSession sess d inputEvent of
                (NoEvent, _) -> multiStepperNoTrigger tailSessions tick
                (oe@(OutputEvent {oeNext}), newSess) -> do
                  tell oe
                  newTail <- multiStepperNoTrigger tailSessions tick
                  return $ PriorityMap.insert newTail (oeNext `after` tick) sessKey (newSess, tick)
            Nothing ->
              error $
                "timing inconsistency multiStepperNoTrigger for "
                  <> show sessKey
                  <> " wanting to step from "
                  <> show oldSessTimestamp
                  <> " to "
                  <> show tick
  where
    inputEvent = InputEvent {ieTick = tick, ieTrigger = Nothing}

-- | Internal function for multiStepper, factored out for clarity
multiStepperForTrigger :: MultiSession -> Timestamp -> (SessionKey, SimulationSetChargingProfile) -> (OutputEvent [] SessionOutput, MultiSession)
multiStepperForTrigger allSessions tick (sessKey, setChargingProf) = case PriorityMap.remove allSessions sessKey of
  Just ((_, (sess, oldSessTimestamp)), otherSessions) ->
    case oldSessTimestamp `maybeDurationUntil` tick of
      Just duration -> do
        case stepSession sess duration $ InputEvent {ieTick = tick, ieTrigger = Just setChargingProf} of
          (NoEvent, _) -> return otherSessions
          (oe@(OutputEvent {oeNext}), newSess) -> do
            tell oe
            newOtherSessions <- multiStepperNoTrigger otherSessions tick
            return $ PriorityMap.insert newOtherSessions (oeNext `after` tick) sessKey (newSess, tick)
      Nothing ->
        error $
          "timing inconsistency in multiStepperForTrigger for "
            <> show sessKey
            <> " wanting to step from "
            <> show oldSessTimestamp
            <> " to "
            <> show tick
  Nothing -> return allSessions

multiSimulate ::
  (Monad m) =>
  -- | Update 'Session'\'s state (assumed to be at 'InputEvent'.'ieTick' - 'Duration') to become the state
  -- at 'InputEvent'.'ieTick'.
  (MultiSession -> InputEvent i -> (OutputEvent [] e, MultiSession)) ->
  -- | Monadic action delivering the output events that were generated while "stepping" the 'Session'
  (e -> m ()) ->
  -- | Simulation function to call with the updated 'Session' with the 'OutputEvent'.'oeNext' timestamp
  (MultiSimulator -> InputEvent i -> m ()) ->
  MultiSimulator ->
  InputEvent i ->
  m ()
multiSimulate stepper deliverEvent nextSim (MultiSimulator state) inputEvent@(InputEvent {ieTick}) =
  case stepper state inputEvent of
    (NoEvent, remainingSessions) ->
      trace ("NoEvent in multiSimulate @ " <> show ieTick <> ", " <> show (length remainingSessions) <> " sessions left") $
        continueSim Nothing remainingSessions
    (OutputEvent {oeNext, oeEvent}, newState) -> do
      forM_ oeEvent deliverEvent
      continueSim (Just $ oeNext `after` ieTick) newState
  where
    continueSim suggestedNextTick sessions =
      let nextTick = getMin <$> (Min <$> suggestedNextTick) <> getMultiSessionNextTick sessions
       in case nextTick of
            Nothing -> trace "No next tick -> stopping sim" $ return ()
            Just nt -> nextSim (MultiSimulator sessions) $ InputEvent {ieTick = nt, ieTrigger = Nothing}
    getMultiSessionNextTick :: MultiSession -> Maybe (Min Timestamp)
    getMultiSessionNextTick sessions = (\(nextPriority, _, _) -> Min nextPriority) <$> PriorityMap.lookupFirst sessions

multiSimulator :: (MonadIO m) => Config -> (MultiSimulator -> InputEvent (SessionKey, SimulationSetChargingProfile) -> m ()) -> MultiSimulator -> InputEvent (SessionKey, SimulationSetChargingProfile) -> m ()
multiSimulator config = multiSimulate multiStepper (deliverSessionOutputIO config)

recurseMultiSimulation ::
  (MonadIO m) =>
  Config ->
  -- | Conversion function from current (wall clock time) to simulation timestamps
  (UTCTime -> Timestamp) ->
  -- | When is simulation finished?
  Timestamp ->
  -- | Channel to query for next event from external source
  MVar SimulationSetChargingProfile ->
  -- | Starting state of next simulation step
  MultiSimulator ->
  -- | Next input event in case no external event arrives first
  InputEvent (SessionKey, SimulationSetChargingProfile) ->
  m ()
recurseMultiSimulation config wallClockToTimestamp finalTimestamp eventChannel simState evt@(InputEvent {ieTick, ieTrigger}) = do
  now <- wallClockToTimestamp <$> liftIO getCurrentTime
  -- When waiting for an external event, avoid the situation where the
  -- external event falls exactly at the same time as evt's ieTrigger and
  -- evt's ieTrigger gets lost.
  let externalEventDeadline = case ieTrigger of
        Just _ -> milliseconds 1 `before` ieTick
        Nothing -> ieTick
  liftIO $ putStrLn $ "now=" <> show now <> ", externalEventDeadline=" <> show externalEventDeadline <> ", simState=" <> show simState <> ", evt=" <> show evt
  let continueSimulation = multiSimulator config (recurseMultiSimulation config wallClockToTimestamp finalTimestamp eventChannel) simState
  if now < finalTimestamp
    then case now `maybeDurationUntil` min externalEventDeadline finalTimestamp of
      Just duration ->
        getNextEvent
          wallClockToTimestamp
          eventChannel
          (duration `after` now)
          duration
          (evt {ieTrigger = snd <$> ieTrigger})
          >>= continueSimulation . mapEventToSession simState
      Nothing -> continueSimulation evt
    else liftIO $ putStrLn $ "Simulation done at " <> show now
  where
    mapEventToSession :: MultiSimulator -> InputEvent SimulationSetChargingProfile -> InputEvent (SessionKey, SimulationSetChargingProfile)
    mapEventToSession _ InputEvent {ieTick = ii, ieTrigger = Nothing} = InputEvent {ieTick = ii, ieTrigger = Nothing}
    mapEventToSession
      (MultiSimulator allSessions)
      InputEvent {ieTick = ii, ieTrigger = tt@(Just (SimulationSetChargingProfile {scpTransactionId}))} =
        case find (matchTransactionId scpTransactionId) $ PriorityMap.listContents allSessions of
          Nothing -> InputEvent {ieTick = ii, ieTrigger = Nothing}
          Just (_, k, _) -> InputEvent {ieTick = ii, ieTrigger = fmap (k,) tt}
    matchTransactionId :: TransactionId -> (Timestamp, SessionKey, (Session, Timestamp)) -> Bool
    matchTransactionId xctnId (_, _, (Session _ Charging {transactionId}, _)) = xctnId == transactionId
    matchTransactionId _ _ = False

-- | Start a simulation of several charging sessions on different charging stations in parallel
startMultiSimulation ::
  (MonadIO m) =>
  -- | Configuration
  Config ->
  -- | Channel from the OCPI 2.2.1 API to the simulator to relay the SCSP's SetChargingProfile calls
  MVar SimulationSetChargingProfile ->
  -- | The 'Session's to simulate
  MultiSession ->
  -- | Starting time of the simulation (in the simulator time frame)
  Timestamp ->
  -- | Simulation will stop after this amount of time
  Duration ->
  m ()
startMultiSimulation config eventChannel allSessions t0 duration = do
  refTime <- liftIO getCurrentTime -- refTime [wall clock time] corresponds to t0 [simulation time]
  let wallClockToTimestamp = utcTimeToSimulationTime t0 refTime
  let firstTick = case PriorityMap.lookupFirst allSessions of
        Just (nextTimestamp, _, _) -> nextTimestamp
        Nothing -> duration `after` t0
  multiSimulator
    config
    (recurseMultiSimulation config wallClockToTimestamp (duration `after` t0) eventChannel)
    (MultiSimulator allSessions)
    (InputEvent {ieTick = firstTick, ieTrigger = Nothing})

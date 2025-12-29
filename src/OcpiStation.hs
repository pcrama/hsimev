{-# LANGUAGE Safe #-}

module OcpiStation
  ( Config (..),
    deliverSessionOutputIO,
    parseSetChargingProfile,
    startSimulation,
  )
where

import ChargingStation
import Control.Concurrent (MVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Ocpi221 qualified as O
import System.Timeout qualified as SysTimeout
import Prelude

type Config :: Type
data Config = Config {scspBaseUrl :: !Text, scspToken :: !String}
  deriving stock (Show)

encodeSimulationTimestampForOcpi :: Timestamp -> Text
encodeSimulationTimestampForOcpi ts = formatted <> "Z"
  where
    utct0 = UTCTime {utctDay = ModifiedJulianDay 0, utctDayTime = 0}
    utct = addUTCTime (realToFrac $ secondsFrom $ clampingDurationUntil (Timestamp 0) ts) utct0
    formatted = pack $ take 23 $ formatTime defaultTimeLocale "%FT%T%Q" utct

deliverSessionOutputIO :: (MonadIO m) => Config -> SessionOutput -> m ()
deliverSessionOutputIO config so = case so of
  StartCharge timestamp transactionId stationId connectorId -> putSession $ makeStartSession timestamp transactionId stationId connectorId
  SendMeterValues meterValues -> putSession $ makeMeterValuesSession meterValues
  AcceptSetChargingProfile cbUrl -> putCallback cbUrl O.CprtAccepted
  RejectSetChargingProfile cbUrl -> putCallback cbUrl O.CprtRejected
  TimeoutSetChargingProfile cbUrl -> putCallback cbUrl O.CprtUnknown -- TODO
  EndOfCharge meterValues ->
    let session = makeMeterValuesSession meterValues
     in putSession $ session {O.ocpi_session_status = "COMPLETED", O.ocpi_session_end_date_time = pure $ O.ocpi_session_last_updated session}
  where
    putSession = void . O.putSessionRequestIO (scspBaseUrl config) (scspToken config)
    putCallback cpUrl cprt = void $ O.postCallbackRequestIO cpUrl cprt
    makeStartSession timestamp (TransactionId transactionId) stationId connectorId =
      O.Session
        { O.ocpi_session_country_code = "NL",
          O.ocpi_session_party_id = "LMS",
          O.ocpi_session_id = transactionId,
          O.ocpi_session_start_date_time = encodeSimulationTimestampForOcpi timestamp,
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
          O.ocpi_session_last_updated = encodeSimulationTimestampForOcpi timestamp
        }
    makeMeterValuesSession
      ( MeterValues
          { mvTransactionId = TransactionId transactionId,
            mvStationId,
            mvConnectorId,
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
            O.ocpi_session_start_date_time = encodeSimulationTimestampForOcpi mvTimestamp,
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
            O.ocpi_session_last_updated = encodeSimulationTimestampForOcpi mvTimestamp
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

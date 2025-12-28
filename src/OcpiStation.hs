{-# LANGUAGE Safe #-}

module OcpiStation
  ( deliverSessionOutputIO,
  )
where

import ChargingStation
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Ocpi221 qualified as O
import Prelude

encodeSimulationTimestampForOcpi :: Timestamp -> Text
encodeSimulationTimestampForOcpi ts = formatted <> "Z"
  where
    utct0 = UTCTime {utctDay = ModifiedJulianDay 0, utctDayTime = 0}
    utct = addUTCTime (realToFrac $ secondsFrom $ clampingDurationUntil (Timestamp 0) ts) utct0
    formatted = pack $ take 23 $ formatTime defaultTimeLocale "%FT%T%Q" utct

deliverSessionOutputIO :: (MonadIO m) => SessionOutput -> m ()
deliverSessionOutputIO = void . O.putSessionRequestIO . makeOcpi221Session

makeOcpi221Session :: SessionOutput -> O.Session
makeOcpi221Session (StartCharge timestamp (TransactionId transactionId) stationId connectorId) =
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
            O.ocpi_cdr_id = "cdr_ID",
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
makeOcpi221Session
  ( SendMeterValues
      ( MeterValues
          { mvTransactionId = TransactionId transactionId,
            mvStationId,
            mvConnectorId,
            mvTimestamp,
            mvCurrents,
            mvOfferedCurrent,
            mvEnergyDelivered
          }
        )
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
              O.ocpi_cdr_id = "cdr_ID",
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

-- makeOcpi221Session (AcceptSetChargingProfile cb) =

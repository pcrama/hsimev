import qualified Data.Text as T
import Ocpi221
import Web.Scotty

parseSetChargingProfile :: Int -> SetChargingProfile -> Maybe (Int, Double, T.Text)
parseSetChargingProfile
  sessionId
  ( SetChargingProfile
      { response_url,
        charging_profile =
          ChargingProfile
            { charging_rate_unit = "A",
              charging_profile_period = [ChargingProfilePeriod {limit}]
            }
      }
    ) = Just (sessionId, limit, response_url)
parseSetChargingProfile _ _ = Nothing

main = do
  r <- putSessionRequestIO $ Session {
    ocpi_session_country_code="NL",
    ocpi_session_party_id="LMS",
    ocpi_session_id="1234554321",
    ocpi_session_start_date_time="2025-12-25T12:26:54Z",
    ocpi_session_end_date_time=Nothing,
    ocpi_session_kwh=666.66,
    ocpi_session_cdr_token=CdrToken {
                                        ocpi_cdr_country_code="NL",
                                        ocpi_cdr_party_id="LMS",
                                        ocpi_cdr_id="cdr_id",
                                        ocpi_cdr_token_type="RFID",
                                        ocpi_cdr_contract_id="contract_id"
                                    },
    -- | Method used for authentication. This might change during a session: AUTH_REQUEST | COMMAND | WHITELIST
    ocpi_session_auth_method="COMMAND",
    ocpi_session_location_id="loc_id",
    ocpi_session_evse_uid="evse_uid",
    ocpi_session_connector_id="connector_id",
    ocpi_session_currency="EUR",
    ocpi_session_status="ACTIVE",
    ocpi_session_last_updated="2025-12-25T18:19:20.213Z"
                                }
  print r
  scotty 3000 $ do
    put "/ocpi/2.2.1/chargingprofiles/:session_id" $ do
      sessionId <- pathParam "session_id"
      setChargingProfile <- jsonData
      case parseSetChargingProfile sessionId setChargingProfile of
        Just (sessionId, limit, response_url) -> do
          liftIO $ print (sessionId, limit, response_url)
          r <- successResponseIO $ ChargingProfileResponse {result = "ACCEPTED", timeout = 300}
          json r
        Nothing -> do
          r <- errorResponseIO $ ChargingProfileResponse {result = "NOT_SUPPORTED", timeout = 300}
          json r

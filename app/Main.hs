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

main = scotty 3000 $ do
  put "/ocpi/2.2/chargingprofiles/:session_id" $ do
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

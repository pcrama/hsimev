{-# LANGUAGE Trustworthy #-}

-- | Minimal implementation: OCPI 2.2.1 standard is *NOT* a goal, just enough
-- adherence to interoperate for the narrow use-case of the simulations I want
-- to run.
module Ocpi221
  ( SetChargingProfile (..),
    ChargingProfile (..),
    ChargingProfilePeriod (..),
    ChargingProfileResponse (..),
    ChargingProfileResultType (..),
    Session (..),
    CdrToken (..),
    encodeTimeAsRFC3339,
    successResponse,
    successResponseIO,
    errorResponse,
    errorResponseIO,
    postCallbackRequestIO,
    putSessionRequest,
    putSessionRequestIO,
    fromWhTo_kWh,
    toStringConcat, -- for testing purposes
    toStringJoin, -- for testing purposes
    urlJoin, -- for testing purposes
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    ToJSON (toJSON),
    encode,
    object,
  )
import Data.ByteString.Lazy qualified as BS
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Network.HTTP
  ( HeaderName (..),
    Request (..),
    RequestMethod (..),
    Response (..),
    mkRequest,
    replaceHeader,
    simpleHTTP,
  )
import Network.Stream (ConnError (..), Result)
import Network.URI (parseURI)
import Prelude

-- | Return kWh rounded to nearest Wh
fromWhTo_kWh :: Double -> Double
fromWhTo_kWh = (* 0.001) . fromIntegral @Int . round

-- | SetChargingProfile comes from OCPI 2.2.1
-- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_charging_profiles.asciidoc#mod_charging_profiles_set_charging_profile_object
type SetChargingProfile :: Type
data SetChargingProfile = SetChargingProfile
  { charging_profile :: ChargingProfile,
    response_url :: T.Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

-- | ChargingProfile comes from OCPI 2.2.1
-- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_charging_profiles.asciidoc#mod_charging_profiles_charging_profile_class
type ChargingProfile :: Type
data ChargingProfile = ChargingProfile
  { -- | Starting point of an absolute profile. If absent the profile will be
    -- relative to start of charging.
    start_date_time :: Maybe T.Text,
    -- | Duration of the charging profile in seconds. If the duration is left
    -- empty, the last period will continue indefinitely or until end of the
    -- transaction in case start_date_time is absent.
    duration :: Maybe Int,
    -- | "W" or "A" but I'm not going to create a data type with those constructors...
    charging_rate_unit :: T.Text,
    -- | Minimum charging rate supported by the EV. The unit of measure is
    -- defined by the chargingRateUnit. This parameter is intended to be
    -- used by a local smart charging algorithm to optimize the power
    -- allocation for in the case a charging process is inefficient at lower
    -- charging rates. Accepts at most one digit fraction (e.g. 8.1)
    min_charging_rate :: Maybe Double,
    -- | List of ChargingProfilePeriod elements defining maximum power or
    -- current usage over time.
    charging_profile_period :: [ChargingProfilePeriod]
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

-- | ChargingProfilePeriod comes from OCPI 2.2.1
-- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_charging_profiles.asciidoc#mod_charging_profiles_charging_profile_period_class
type ChargingProfilePeriod :: Type
data ChargingProfilePeriod = ChargingProfilePeriod
  { -- | Start of the period, in seconds from the start of profile. The value
    -- of StartPeriod also defines the stop time of the previous period.
    start_period :: Int,
    -- | Charging rate limit during the profile period, in the applicable
    -- chargingRateUnit, for example in Amperes (A) or Watts (W). Accepts at
    -- most one digit fraction (e.g. 8.1).
    limit :: Double
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

-- | ChargingProfileResponse comes from OCPI 2.2.1
-- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_charging_profiles.asciidoc#mod_charging_profiles_response_object
type ChargingProfileResponse :: Type
data ChargingProfileResponse = ChargingProfileResponse
  { -- | Response from the CPO on the ChargingProfile request: (ACCEPTED |
    -- NOT_SUPPORTED | REJECTED | TOO_OFTEN | UNKNOWN_SESSION)
    result :: T.Text,
    -- | Timeout for this ChargingProfile request in seconds. When the Result
    -- is not received within this timeout, the eMSP can assume that the
    -- message might never be sent.
    timeout :: Int
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

-- | https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/transport_and_format.asciidoc#117-response-format
type ResponseFormat :: Type -> Type
data ResponseFormat a = ResponseFormat
  { data_ :: a,
    status_code :: Int,
    status_message :: Maybe T.Text,
    timestamp :: T.Text
  }
  deriving stock (Show, Eq)

instance (ToJSON a) => ToJSON (ResponseFormat a) where
  toJSON (ResponseFormat {data_, status_code, status_message, timestamp}) = object $ mkAttrValList status_message
    where
      mkAttrValList Nothing = ["data" .= toJSON data_, "status_code" .= status_code, "timestamp" .= timestamp]
      mkAttrValList (Just txt) = ("status_message" .= txt) : mkAttrValList Nothing

successResponse :: a -> T.Text -> ResponseFormat a
successResponse a ts = ResponseFormat {data_ = a, status_code = 1000, status_message = Nothing, timestamp = ts}

errorResponse :: a -> T.Text -> ResponseFormat a
errorResponse a ts = ResponseFormat {data_ = a, status_code = 2000, status_message = Just "Generic Error!", timestamp = ts}

successResponseIO :: (MonadIO m) => a -> m (ResponseFormat a)
successResponseIO = withTimestamp . successResponse

errorResponseIO :: (MonadIO m) => a -> m (ResponseFormat a)
errorResponseIO = withTimestamp . errorResponse

withTimestamp :: (MonadIO m) => (T.Text -> v) -> m v
withTimestamp = (<$> liftIO getCurrentTimeAsRFC3339)

-- | Source - https://stackoverflow.com/a/73646742 and https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/types.asciidoc#types_datetime_type
-- Posted by beyarkay, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-12-25, License - CC BY-SA 4.0
encodeTimeAsRFC3339 :: UTCTime -> T.Text
encodeTimeAsRFC3339 t =
  -- Take the first 23 characters so we don't get the microseconds
  let val = T.pack $ take 23 $ formatTime defaultTimeLocale "%FT%T%Q" t
   in val <> "Z"

getCurrentTimeAsRFC3339 :: IO T.Text
getCurrentTimeAsRFC3339 = encodeTimeAsRFC3339 <$> getCurrentTime

-- | https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_sessions.asciidoc#mod_sessions_session_object
type Session :: Type
data Session = Session
  { -- | ISO-3166 alpha-2 country code of the MSP that 'owns' this Token.
    ocpi_session_country_code :: T.Text,
    -- | ID of the eMSP that 'owns' this Token (following the ISO-15118 standard).
    ocpi_session_party_id :: T.Text,
    -- | The unique id that identifies the charging session in the CPO platform.
    ocpi_session_id :: T.Text,
    -- | The timestamp when the session became ACTIVE in the Charge Point.  When
    -- the session is still PENDING, this field SHALL be set to the time the
    -- Session was created at the Charge Point. When a Session goes from PENDING
    -- to ACTIVE, this field SHALL be updated to the moment the Session went to
    -- ACTIVE in the Charge Point.
    ocpi_session_start_date_time :: T.Text,
    -- | The timestamp when the session was completed/finished, charging might
    -- have finished before the session ends, for example: EV is full, but
    -- parking cost also has to be paid.
    ocpi_session_end_date_time :: Maybe T.Text,
    -- | How many kWh were charged.
    ocpi_session_kwh :: Double,
    -- | Token used to start this charging session, including all the relevant
    -- information to identify the unique token.
    ocpi_session_cdr_token :: CdrToken,
    -- | Method used for authentication. This might change during a session: AUTH_REQUEST | COMMAND | WHITELIST
    ocpi_session_auth_method :: T.Text,
    -- authorization_reference :: T.Text -- Optional and not useful for my simulation -> skip

    -- | Location.id of the Location object of this CPO, on which the charging
    -- session is/was happening.
    ocpi_session_location_id :: T.Text,
    -- | EVSE.uid of the EVSE of this Location on which the charging session
    -- is/was happening. Allowed to be set to: #NA when this session is created
    -- for a reservation, but no EVSE yet assigned to the driver.
    ocpi_session_evse_uid :: T.Text,
    -- | Connector.id of the Connector of this Location where the charging
    -- session is/was happening. Allowed to be set to: #NA when this session is
    -- created for a reservation, but no connector yet assigned to the driver.
    ocpi_session_connector_id :: T.Text,
    -- meter_id :: T.Text -- Optional and not useful for my simulation -> skip

    -- | ISO 4217 code of the currency used for this session.
    ocpi_session_currency :: T.Text,
    -- charging_periods :: [ChargingPeriods] -- Optional and not useful for my simulation -> skip
    -- total_cost :: Double -- Optional and not useful for my simulation -> skip

    -- | The status of the session: ACTIVE | COMPLETED | INVALID | PENDING | RESERVATION
    ocpi_session_status :: T.Text,
    -- | Timestamp when this Session was last updated (or created).
    ocpi_session_last_updated :: T.Text
  }
  deriving stock (Show, Eq)

instance ToJSON Session where
  toJSON
    ( Session
        { ocpi_session_country_code,
          ocpi_session_party_id,
          ocpi_session_id,
          ocpi_session_start_date_time,
          ocpi_session_end_date_time,
          ocpi_session_kwh,
          ocpi_session_cdr_token,
          ocpi_session_auth_method,
          ocpi_session_location_id,
          ocpi_session_evse_uid,
          ocpi_session_connector_id,
          ocpi_session_currency,
          ocpi_session_status,
          ocpi_session_last_updated
        }
      ) =
      object $ prefixAsDlist . skipNothing "end_date_time" ocpi_session_end_date_time $ jsonTail
      where
        prefixAsDlist tl =
          ("country_code" .= toJSON ocpi_session_country_code)
            : ("party_id" .= toJSON ocpi_session_party_id)
            : ("id" .= toJSON ocpi_session_id)
            : ("start_date_time" .= toJSON ocpi_session_start_date_time)
            : tl
        skipNothing _ Nothing tl = tl
        skipNothing key (Just val) tl = (key .= toJSON val) : tl
        jsonTail =
          [ "kwh" .= toJSON ocpi_session_kwh,
            "cdr_token" .= toJSON ocpi_session_cdr_token,
            "auth_method" .= toJSON ocpi_session_auth_method,
            "location_id" .= toJSON ocpi_session_location_id,
            "evse_uid" .= toJSON ocpi_session_evse_uid,
            "connector_id" .= toJSON ocpi_session_connector_id,
            "currency" .= toJSON ocpi_session_currency,
            "status" .= toJSON ocpi_session_status,
            "last_updated" .= toJSON ocpi_session_last_updated
          ]

-- | https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_cdrs.asciidoc#mod_cdrs_cdr_token_object
type CdrToken :: Type
data CdrToken = CdrToken
  { -- | ISO-3166 alpha-2 country code of the MSP that 'owns' this Token.
    ocpi_cdr_country_code :: T.Text,
    -- | ID of the eMSP that 'owns' this Token (following the ISO-15118 standard).
    ocpi_cdr_party_id :: T.Text,
    -- | Unique ID by which this Token can be identified.  This is the field
    -- used by the CPO’s system (RFID reader on the Charge Point) to identify
    -- this token.  Currently, in most cases: type=RFID, this is the RFID hidden
    -- ID as read by the RFID reader, but that is not a requirement.  If this is
    -- a type=APP_USER Token, it will be a unique, by the eMSP, generated ID.
    ocpi_cdr_uid :: T.Text,
    -- | AD_HOC_USER | APP_USER | OTHER | RFID
    ocpi_cdr_token_type :: T.Text,
    -- | Uniquely identifies the EV driver contract token within the eMSP’s
    -- platform (and suboperator platforms). Recommended to follow the
    -- specification for eMA ID from "eMI3 standard version V1.0"
    ocpi_cdr_contract_id :: T.Text
  }
  deriving stock (Show, Eq)

instance ToJSON CdrToken where
  toJSON
    ( CdrToken
        { ocpi_cdr_country_code,
          ocpi_cdr_party_id,
          ocpi_cdr_uid,
          ocpi_cdr_token_type,
          ocpi_cdr_contract_id
        }
      ) =
      object
        [ "country_code" .= toJSON ocpi_cdr_country_code,
          "party_id" .= toJSON ocpi_cdr_party_id,
          "uid" .= toJSON ocpi_cdr_uid,
          "token_type" .= toJSON ocpi_cdr_token_type,
          "contract_id" .= toJSON ocpi_cdr_contract_id
        ]

-- | https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_charging_profiles.asciidoc#166-chargingprofileresulttype-enum
type ChargingProfileResultType :: Type
data ChargingProfileResultType = CprtAccepted | CprtRejected | CprtUnknown
  deriving stock (Show, Eq)

instance ToJSON ChargingProfileResultType where
  toJSON CprtAccepted = "ACCEPTED"
  toJSON CprtRejected = "REJECTED"
  toJSON CprtUnknown = "UNKNOWN"

postCallbackRequestIO :: (MonadIO m) => T.Text -> ChargingProfileResultType -> m (Result (Response BS.ByteString))
postCallbackRequestIO callbackUrl cprt = do
  liftIO $ case parseURI $ T.unpack callbackUrl of
    Nothing -> do
      let msg = T.unpack $ "Unable to build HTTP POST request for callbackUrl " <> callbackUrl
      putStrLn msg
      return $ Left $ ErrorMisc msg
    Just url -> do
      body <- encode <$> successResponseIO cprt
      let req =
            replaceHeader HdrContentType "application/json"
              . replaceHeader HdrContentLength (show $ BS.length body)
              $ (mkRequest POST url :: Request BS.ByteString) {rqBody = body}
      putStrLn $ "Making HTTP request " <> show req <> "with body " <> show (rqBody req)
      simpleHTTP req

urlJoin :: T.Text -> String -> String
urlJoin prefixMbSlash suffixMbSlash = T.foldr (:) suffixWithSlash prefix
  where
    prefix = case T.unsnoc prefixMbSlash of
      Just (p, '/') -> p
      _ -> prefixMbSlash
    suffixWithSlash = case suffixMbSlash of
      s@('/' : _) -> s
      s -> '/' : s

toStringConcat :: [T.Text] -> String -> String
toStringConcat texts finalTail = foldr unpackConcat finalTail texts
  where
    unpackConcat txt tl = T.foldr (:) tl txt

toStringJoin :: Char -> [T.Text] -> String -> String
toStringJoin sep texts finalTail = foldr unpackConcat (sep : finalTail) texts
  where
    unpackConcat txt tl = sep : T.foldr (:) tl txt

-- | Build "PUT" request to inform SCSP about Session object
--
-- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_sessions.asciidoc#put-method-1
putSessionRequest ::
  -- | URL where to "PUT" the Session information without the
  -- `/sessions/{country_code}/{party_id}/{session_id}` suffix, eg "https://example.com/ocpi/emsp/2.2.1"
  T.Text ->
  -- | API token: "Token some-super-secret-value"
  --
  -- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/transport_and_format.asciidoc#112-authorization-header
  String ->
  -- | Session object to "PUT"
  Session ->
  Maybe (Request BS.ByteString)
putSessionRequest urlPutSession apiToken sess@(Session {ocpi_session_country_code, ocpi_session_party_id, ocpi_session_id}) = do
  let urlString = urlJoin urlPutSession $ toStringJoin '/' ["sessions", ocpi_session_country_code, ocpi_session_party_id] $ T.unpack ocpi_session_id
  let body = encode sess
  url <- parseURI urlString
  return
    $ replaceHeader HdrContentType "application/json"
      . replaceHeader HdrContentLength (show $ BS.length body)
      . replaceHeader HdrAuthorization apiToken
    $ (mkRequest PUT url :: Request BS.ByteString) {rqBody = body}

-- | Make HTTP "PUT" request to inform SCSP about Session object
--
-- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/mod_sessions.asciidoc#put-method-1
putSessionRequestIO ::
  (MonadIO m) =>
  -- | URL where to "PUT" the Session information without the
  -- `/{country_code}/{party_id}/{session_id}` suffix
  T.Text ->
  -- | API token: "Token some-super-secret-value"
  --
  -- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/transport_and_format.asciidoc#112-authorization-header
  String ->
  -- | Session object to "PUT"
  Session ->
  m (Result (Response BS.ByteString))
putSessionRequestIO urlPutSession apiToken sess = do
  liftIO $ case putSessionRequest urlPutSession apiToken sess of
    Nothing -> do
      let msg = "Unable to build HTTP PUT request for session"
      putStrLn msg
      return $ Left $ ErrorMisc msg
    Just req -> do
      putStrLn $ "Making HTTP request " <> show req <> "with body " <> show (rqBody req)
      simpleHTTP req

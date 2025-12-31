{-# LANGUAGE Trustworthy #-}

module Ocpi221Tests (ocpi221Tests) where

import Data.Text (Text)
import Network.HTTP (HeaderName (..), Request (..), hdrName, hdrValue)
import Network.URI (URI (..), URIAuth (..), nullURI, nullURIAuth)
import Ocpi221
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

ocpi221Tests =
  [ ocpi221ToStringConcatTests,
    ocpi221ToStringJoinTests,
    ocpi221UrlJoinTests,
    ocpi221FromWhTo_kWhTests,
    ocpi221PostCallbackRequest,
    ocpi221PutSessionRequest
  ]

ocpi221FromWhTo_kWhTests :: TestTree
ocpi221FromWhTo_kWhTests =
  testGroup
    "fromWhTo_kWh"
    [ testCase (show input <> "Wh -> " <> show expectedOutput <> "kWh") $ fromWhTo_kWh input @?= expectedOutput
    | (input, expectedOutput) <-
        [ (1000, 1),
          (1, 0.001),
          (0.1, 0),
          (12345.678, 12.346),
          ((-875), (-0.875))
        ]
    ]

ocpi221UrlJoinTests :: TestTree
ocpi221UrlJoinTests =
  testGroup
    "urlJoin"
    [ testCase "prefix + suffix" $ urlJoin "prefix" "suffix" @?= "prefix/suffix",
      testCase "prefix-with-slash/ + suffix" $
        urlJoin "prefix-with-slash/" "suffix" @?= "prefix-with-slash/suffix",
      testCase "prefix-with-slash/ + /suffix-with-slash" $
        urlJoin "prefix-with-slash/" "/suffix-with-slash" @?= "prefix-with-slash/suffix-with-slash",
      testCase "prefix + /suffix/with/slash/" $
        urlJoin "prefix" "/suffix/with/slash/" @?= "prefix/suffix/with/slash/"
    ]

ocpi221ToStringConcatTests :: TestTree
ocpi221ToStringConcatTests =
  testGroup
    "toStringConcat"
    [ testCase "all empty" $ toStringConcat [] "" @?= "",
      testCase "[\"a\", \"b\"] \"c\"" $ toStringConcat ["a", "b"] "c" @?= "abc",
      testCase "lazy in 1st arg" $
        take 9 (toStringConcat (repeat "wx") "y") @?= "wxwxwxwxw",
      testCase "lazy in 2nd arg" $
        take 9 (toStringConcat ["ab", "cd", "ef"] $ repeat 'z') @?= "abcdefzzz"
    ]

ocpi221ToStringJoinTests :: TestTree
ocpi221ToStringJoinTests =
  testGroup
    "toStringJoin"
    [ testCase "all empty" $ toStringJoin ':' [] "" @?= ":",
      testCase "[\"a\", \"b\"] \"c\"" $ toStringJoin ':' ["a", "b"] "c" @?= ":a:b:c",
      testCase "lazy in 1st arg" $
        take 9 (toStringJoin '/' (repeat "wx") "y") @?= "/wx/wx/wx",
      testCase "lazy in 2nd arg" $
        take 9 (toStringJoin '/' ["ab", "c"] $ repeat 'z') @?= "/ab/c/zzz"
    ]

ocpi221PutSessionRequest :: TestTree
ocpi221PutSessionRequest =
  testGroup
    "putSessionRequest"
    [ testCase "happy case" $ do
        let Just req = putSessionRequest "http://example.com/ocpi2.2.1" "Token api-token" exampleSession
        rqURI req
          @?= nullURI
            { uriScheme = "http:",
              uriAuthority = Just $ nullURIAuth {uriRegName = "example.com"},
              uriPath = "/ocpi2.2.1/sessions/NL/LMS/2134"
            }
        let headers = map (\x -> (hdrName x, hdrValue x)) $ rqHeaders req
        lookup HdrAuthorization headers @?= Just "Token api-token"
        lookup HdrContentType headers @?= Just "application/json",
      testCase "invalid URL" $
        uriAuthority . rqURI
          <$> putSessionRequest "https://invalid url" "Token api-token" exampleSession
          @?= Nothing
    ]
  where
    exampleSession =
      Session
        { ocpi_session_country_code = "NL",
          ocpi_session_party_id = "LMS",
          ocpi_session_id = "2134",
          ocpi_session_start_date_time = "2025-12-29T11:03:04Z",
          ocpi_session_end_date_time = Nothing,
          ocpi_session_kwh = 0,
          ocpi_session_cdr_token =
            CdrToken
              { ocpi_cdr_country_code = "NL",
                ocpi_cdr_party_id = "LMS",
                ocpi_cdr_uid = "cdr_ID",
                ocpi_cdr_token_type = "RFID",
                ocpi_cdr_contract_id = "contract_id"
              },
          ocpi_session_auth_method = "COMMAND",
          ocpi_session_location_id = "loc_id",
          ocpi_session_evse_uid = "stationId",
          ocpi_session_connector_id = "1",
          ocpi_session_currency = "EUR",
          ocpi_session_status = "ACTIVE",
          ocpi_session_last_updated = "2025-12-29T11:03:04Z"
        }

ocpi221PostCallbackRequest :: TestTree
ocpi221PostCallbackRequest =
  testGroup
    "postCallbackRequest"
    [ testCase "happy case" $ do
        let Just req = postCallbackRequest "Token api-token" "https://scsp.com/station_id/session_id" CprtAccepted
        rqURI req
          @?= nullURI
            { uriScheme = "https:",
              uriAuthority = Just $ nullURIAuth {uriRegName = "scsp.com"},
              uriPath = "/station_id/session_id"
            }
        let headers = map (\x -> (hdrName x, hdrValue x)) $ rqHeaders req
        lookup HdrAuthorization headers @?= Just "Token api-token"
        lookup HdrContentType headers @?= Just "application/json"
        rqBody req @?= "{\"result\":\"ACCEPTED\"}",
      testCase "invalid URL" $
        uriAuthority . rqURI
          <$> postCallbackRequest "Token api-token" "bad scsp.com provided invalid URL" CprtAccepted
          @?= Nothing
    ]
  where
    responseFormatBuilder = flip successResponse "2025-12-31T12:34:56Z"

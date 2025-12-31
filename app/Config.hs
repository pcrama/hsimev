module Config (Config (..), simulationServerConfig, defaultConfig) where

import Data.Kind (Type)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Options.Applicative
import Prelude

type Config :: Type
data Config = Config
  { -- | server port
    port :: Int,
    -- | Smart Charging Service Provider OCPI 2.2.1 API's base URL
    scspBaseUrl :: T.Text,
    -- | API token: "Token some-super-secret-value"
    --
    -- https://github.com/ocpi/ocpi/blob/a57ecb624fbe0f19537ac7956a11f3019a65018f/transport_and_format.asciidoc#112-authorization-header
    scspToken :: String,
    -- | how long the simulation should run in [s]
    simulationDuration :: Word64,
    stationId :: T.Text,
    connectorId :: Word8,
    transactionId :: T.Text
  }
  deriving stock (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { port = 3000,
      scspBaseUrl = "http://example.com/ocpi/2.2.1/",
      scspToken = "Token api-test-token",
      simulationDuration = 20 * 60,
      stationId = "station_id",
      connectorId = 1,
      transactionId = "4321234"
    }

simulationServerConfig :: Parser Config
simulationServerConfig =
  Config
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port on which the simulator's API interface is listening"
          <> showDefault
          <> value (port defaultConfig)
          <> metavar "PORT"
      )
    <*> strOption
      ( long "scsp"
          <> short 's'
          <> metavar "BASE_URL"
          <> help "Base URL of the Smart Charging Service Provider receiving the simulated charging session states"
      )
    <*> strOption
      ( long "scsp-token"
          <> metavar "TOKEN_XXX"
          <> help "Token accepted by the Smart Charging Service Provider receiving the simulated charging session states"
          <> showDefault
          <> value (scspToken defaultConfig)
      )
    <*> option
      auto
      ( long "duration"
          <> short 'd'
          <> help "Duration in [s] of the simulation"
          <> showDefault
          <> value (simulationDuration defaultConfig)
          <> metavar "D"
      )
    <*> strOption
      ( long "station-id"
          <> help "Station ID on which the charging session is simulated"
          <> showDefault
          <> value (stationId defaultConfig)
          <> metavar "S"
      )
    <*> option
      auto
      ( long "connector-id"
          <> help "Connector ID on which the charging session is simulated"
          <> showDefault
          <> value (connectorId defaultConfig)
          <> metavar "I"
      )
    <*> strOption
      ( long "transaction-id"
          <> help "Charging session ID"
          <> showDefault
          <> value (transactionId defaultConfig)
          <> metavar "T"
      )

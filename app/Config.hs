module Config (Config (..), simulationServerConfig, defaultConfig) where

import Data.Kind (Type)
import Data.Text qualified as T
import Data.Word (Word64)
import Options.Applicative
import Prelude

type Config :: Type
data Config = Config
  { -- | server port
    port :: Int,
    -- | Smart Charging Service Provider OCPI 2.2.1 API's base URL
    scspBaseUrl :: T.Text,
    -- | how long the simulation should run in [s]
    simulationDuration :: Word64
  }
  deriving stock (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { port = 3000,
      scspBaseUrl = "http://example.com/ocpi/2.2.1/",
      simulationDuration = 20 * 60
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
    <*> option
      auto
      ( long "duration"
          <> short 'd'
          <> help "Duration in [s] of the simulation"
          <> showDefault
          <> value (simulationDuration defaultConfig)
          <> metavar "D"
      )

{-# LANGUAGE Trustworthy #-}

import ChargingStation
import Config qualified
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Time.Clock (getCurrentTime)
import Ocpi221 qualified as O
import OcpiStation qualified as OS
import Options.Applicative
import Web.Scotty (json, jsonData, pathParam, put, scotty)
import Prelude

apiServer :: Int -> MVar SimulationSetChargingProfile -> IO ()
apiServer port ch = scotty port $ do
  put "/ocpi/2.2/chargingprofiles/:session_id" $ do
    sessionId <- pathParam "session_id"
    setChargingProfile <- jsonData
    case OS.parseSetChargingProfile (TransactionId sessionId) setChargingProfile of
      Just simSetChgProf -> do
        liftIO $ print simSetChgProf
        liftIO $ putMVar ch simSetChgProf
        r <- O.successResponseIO $ O.ChargingProfileResponse {O.result = "ACCEPTED", O.timeout = 300}
        json r
      Nothing -> do
        r <- O.errorResponseIO $ O.ChargingProfileResponse {O.result = "NOT_SUPPORTED", O.timeout = 300}
        json r

main :: IO ()
main = execParser opts >>= runSimulationAndApi
  where
    opts =
      info
        (Config.simulationServerConfig <**> helper)
        ( fullDesc
            <> progDesc
              "Simulate charging points and their sessions forwarding \
              \their information to an SCSP & accepting SetChargingProfile \
              \from the SCSP."
            <> header "hsimev - simulator for SCSP development"
        )

runSimulationAndApi :: Config.Config -> IO ()
runSimulationAndApi config = do
  ch <- newEmptyMVar
  putStrLn $ "Forking API server on port " <> show (Config.port config)
  currentTime <- getCurrentTime
  let simConfig =
        OS.Config
          { OS.scspBaseUrl = Config.scspBaseUrl config,
            OS.scspToken = Config.scspToken config,
            OS.timestampToUtcTime = simulationTimeToUTCTime startTime currentTime
          }
  putStrLn $ "Starting simulation with config " <> show simConfig
  void $ forkIO $ apiServer (Config.port config) ch
  OS.startSimulation simConfig ch (Session sessConf sessState) startTime $ seconds $ Config.simulationDuration config
  where
    startTime = Timestamp 0
    meterValuesPeriodicity = seconds 60
    sessConf =
      SessionConfiguration
        { batteryCapacity = 80000.0,
          sessionTarget = LeaveAtTick $ minutes 20 `after` startTime,
          charge = chargeEfficientlyUntil80Percent sessConf,
          getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 16 0.0 sessConf,
          phases = RST,
          stationId = "id_station",
          connectorId = 2,
          meterValuesPeriodicity = meterValuesPeriodicity
        }
    sessState =
      Charging
        { batteryLevel = 16000.0,
          energyDelivered = 0.0,
          currentOffered = 11.5,
          transactionId = TransactionId "4321234",
          startDateTime = startTime,
          meterValuesStateMachine = NextMeterValueSampleDue $ meterValuesPeriodicity `after` startTime
        }

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
import PriorityMap qualified
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
  OS.startMultiSimulation simConfig ch (PriorityMap.fromListContents allSessions) startTime $ seconds $ Config.simulationDuration config
  where
    startTime = Timestamp 123000000
    allSessions =
      [ (dt `after` startTime, makeKey sess, (sess, startTime))
      | (dt, sess) <-
          [ (sess1DelayToNextState, Session sessConf1 sessState1),
            (sess2DelayToNextState, Session sessConf2 sessState2),
            (sess3DelayToNextState, Session sessConf3 sessState3)
          ]
      ]
    meterValuesPeriodicity = seconds 60
    makeKey (Session (SessionConfiguration {stationId, connectorId}) _) = (stationId, connectorId)
    sess1DelayToNextState = seconds 41
    sess2DelayToNextState = seconds 3
    sess3DelayToNextState = seconds 3 <> milliseconds 700
    sessConf1 =
      SessionConfiguration
        { batteryCapacity = 80000.0,
          sessionTarget = LeaveAtTick $ minutes 20 `after` startTime,
          charge = chargeEfficientlyUntil80Percent sessConf1,
          getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 16 0.0 sessConf1,
          phases = RST,
          stationId = Config.stationId config,
          connectorId = Config.connectorId config,
          meterValuesPeriodicity = meterValuesPeriodicity
        }
    sessState1 =
      Charging
        { batteryLevel = 16000.0,
          energyDelivered = 0.0,
          currentOffered = 9,
          transactionId = TransactionId $ Config.transactionId config <> "1",
          startDateTime = startTime,
          meterValuesStateMachine = NextMeterValueSampleDue $ sess1DelayToNextState `after` startTime
        }
    sessConf2 =
      SessionConfiguration
        { batteryCapacity = 60000.0,
          sessionTarget = LeaveAtTick $ minutes 15 `after` startTime,
          charge = chargeEfficientlyUntil80Percent sessConf2,
          getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 14 0.0 sessConf2,
          phases = RS,
          stationId = Config.stationId config,
          connectorId = Config.connectorId config + 1,
          meterValuesPeriodicity = meterValuesPeriodicity
        }
    sessState2 =
      Charging
        { batteryLevel = 46000.0,
          energyDelivered = 2000.0,
          currentOffered = 9,
          transactionId = TransactionId $ Config.transactionId config <> "2",
          startDateTime = minutes 54 `before` startTime,
          meterValuesStateMachine = NextMeterValueSampleDue $ sess2DelayToNextState `after` startTime
        }
    sessConf3 =
      SessionConfiguration
        { batteryCapacity = 40000.0,
          sessionTarget = LeaveAfterBoth (minutes 15 `after` startTime) 35400,
          charge = chargeEfficientlyUntil80Percent sessConf3,
          getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 20 0.0 sessConf2,
          phases = T,
          stationId = Config.stationId config,
          connectorId = Config.connectorId config + 2,
          meterValuesPeriodicity = meterValuesPeriodicity
        }
    sessState3 =
      Charging
        { batteryLevel = 24000.0,
          energyDelivered = 1000.0,
          currentOffered = 9,
          transactionId = TransactionId $ Config.transactionId config <> "3",
          startDateTime = minutes 45 `before` startTime,
          meterValuesStateMachine = NextMeterValueSampleDue $ sess3DelayToNextState `after` startTime
        }

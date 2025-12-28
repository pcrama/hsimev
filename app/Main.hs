{-# LANGUAGE Trustworthy #-}

import Prelude
import ChargingStation
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Ocpi221 qualified as O
import OcpiStation
import Web.Scotty (json, jsonData, pathParam, put, scotty)

apiServer :: MVar SimulationSetChargingProfile -> IO ()
apiServer ch = scotty 3000 $ do
  put "/ocpi/2.2/chargingprofiles/:session_id" $ do
    sessionId <- pathParam "session_id"
    setChargingProfile <- jsonData
    case parseSetChargingProfile (TransactionId sessionId) setChargingProfile of
      Just simSetChgProf -> do
        liftIO $ print simSetChgProf
        liftIO $ putMVar ch simSetChgProf
        r <- O.successResponseIO $ O.ChargingProfileResponse {O.result = "ACCEPTED", O.timeout = 300}
        json r
      Nothing -> do
        r <- O.errorResponseIO $ O.ChargingProfileResponse {O.result = "NOT_SUPPORTED", O.timeout = 300}
        json r

main :: IO ()
main = do
  ch <- newEmptyMVar
  putStrLn "Forking simulation"
  void $ forkIO $ startSimulation ch (Session sessConf sessState) startTime $ minutes 20
  apiServer ch
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
          meterValuesStateMachine = NextMeterValueSampleDue $ meterValuesPeriodicity `after` startTime
        }

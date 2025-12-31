{-# LANGUAGE Trustworthy #-}

module Main (main) where

import ChargingStation
import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.Writer.Strict (runWriter, runWriterT)
import Data.List (foldl')
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
  ( UTCTime (..),
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import FakeSimulation (fakeSimulation)
import Ocpi221Tests (ocpi221Tests)
import OcpiStationTests qualified
import PriorityMapTests qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

-- | round x to n digits
approx :: Int -> Double -> Double
approx n x = fromInteger (round $ x * (10 ^ n)) / (10.0 ^ n)

-- | assert approximate equality
assertApproxEqual :: String -> Int -> Double -> Double -> Assertion
assertApproxEqual prefix n observed expected =
  assertEqual prefix (approx n expected) (approx n observed)

iterateN :: (Monad m) => (a -> m a) -> Word -> a -> m [a]
iterateN f = go []
  where
    go xs 0 _ = return $ reverse xs
    go xs n x = f x >>= go (x : xs) (n - 1)

iterateWriter :: (Show w, Monoid w) => (a -> (w, a)) -> Word -> a -> ([w], [a])
iterateWriter f n a = go ([], [a]) n a
  where
    go (ws, xs) 0 _ = (reverse ws, reverse xs)
    go (ws, xs) n x = let (w, a) = f x in go (w : ws, a : xs) (n - 1) a

main :: IO ()
main = defaultMain $ do
  testGroup
    "All tests"
    [ testGroup
        "CharginStation tests"
        [ testGroup "Manually checked" [manuallyCheckedComputationTests, fakeSimulationTest],
          timeConversionFunctionTests
        ],
      testGroup "Ocpi221 tests" ocpi221Tests,
      testGroup "OcpiStation tests" OcpiStationTests.tests,
      testGroup "PriorityMap tests" PriorityMapTests.tests
    ]

manuallyCheckedComputationTests :: TestTree
manuallyCheckedComputationTests =
  testGroup
    "computation"
    [ testCase "Just before reaching 80% charge" $ do
        let SimState {tick = ts, session = Session _ sess} = states !! 1
        ts @?= Timestamp 60000
        assertApproxEqual "batteryLevel" 3 (batteryLevel sess) 64000.0
        assertApproxEqual "energyDelivered" 3 (energyDelivered sess) 10122.666666,
      testCase "Just after reaching 80% charge" $ do
        let SimState {tick = ts, session = Session _ sess} = states !! 2
        ts @?= Timestamp 120000
        assertApproxEqual "batteryLevel" 3 (batteryLevel sess) 64092.0
        assertApproxEqual "energyDelivered" 3 (energyDelivered sess) 10245.333333,
      testCase "No meter values before 5min" $ do
        forM_ (takeWhile (\(SimState {tick}) -> tick < (meterValuesPeriodicity `after` startTime)) states) $
          \(SimState {tick, session = session@(Session _ (Charging {meterValuesStateMachine = (NextMeterValueSampleDue nextMeterValueTs)}))}) -> do
            assertEqual "Next meter values sample due" (meterValuesPeriodicity `after` startTime) nextMeterValueTs,
      testCase "Meter value sampled after 5min" $ do
        let (SimState {tick, session = session@(Session _ (Charging {meterValuesStateMachine = (Sampled sampleTimestamp sampledValues)}))} : _) =
              dropWhile (\(SimState {tick}) -> tick < (meterValuesPeriodicity `after` startTime)) states
        assertEqual "Sampled at" (meterValuesPeriodicity `after` startTime) sampleTimestamp
        assertApproxEqual "Energy delivered" 3 (mvEnergyDelivered sampledValues) 10609.380,
      testCase "Meter value sampled after 10min" $ do
        let (SimState {tick, session = session@(Session _ (Charging {meterValuesStateMachine = (Sampled sampleTimestamp sampledValues)}))} : _) =
              dropWhile (\(SimState {tick}) -> tick < ((meterValuesPeriodicity <> meterValuesPeriodicity) `after` startTime)) states
        assertEqual "Sampled at" ((meterValuesPeriodicity <> meterValuesPeriodicity) `after` startTime) sampleTimestamp
        assertApproxEqual "Energy delivered" 3 (mvEnergyDelivered sampledValues) 11203.159
    ]
  where
    startTime = Timestamp 0
    meterValuesPeriodicity = minutes 5
    sessConf =
      SessionConfiguration
        { batteryCapacity = 80000.0,
          sessionTarget = LeaveAtTick $ minutes 20 `after` startTime,
          charge = chargeEfficientlyUntil80Percent sessConf,
          getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 16 0.2 sessConf,
          phases = RT,
          stationId = "station_id",
          connectorId = 1,
          meterValuesPeriodicity = meterValuesPeriodicity
        }
    sessState =
      Charging
        { batteryLevel = 64000.0 - 104.266666,
          energyDelivered = 10000.0,
          currentOffered = 20,
          transactionId = TransactionId "1234321",
          startDateTime = startTime,
          meterValuesStateMachine = NextMeterValueSampleDue $ meterValuesPeriodicity `after` startTime
        }
    stepByMinute :: SimState -> (OutputEvent [] SessionOutput, SimState)
    stepByMinute simState@(SimState {tick}) = stepSimulation simState $ minutes 1 `after` tick
    states =
      snd $
        iterateWriter stepByMinute 25 $
          SimState {tick = startTime, session = Session sessConf sessState}

fakeSimulationTest :: TestTree
fakeSimulationTest =
  testGroup "simulation traces" $
    let startTime = Timestamp 1
        meterValuesPeriodicity = minutes 3
        sessConf =
          SessionConfiguration
            { batteryCapacity = 80000.0,
              sessionTarget = LeaveAtTick $ minutes 20 `after` startTime,
              charge = chargeEfficientlyUntil80Percent sessConf,
              getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 16 0.0 sessConf,
              phases = S,
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
              startDateTime = Timestamp 0,
              meterValuesStateMachine = NextMeterValueSampleDue $ meterValuesPeriodicity `after` startTime
            }
        (sessionTrace, eventTrace) =
          fakeSimulation
            stepSession
            (Session sessConf sessState)
            startTime
            [ (minutes 1 <> seconds 2, SimulationSetChargingProfile (TransactionId "4321234") 10 "cb10"),
              (minutes 4 <> seconds 10, SimulationSetChargingProfile (TransactionId "4321234") 8.5 "cb8p5")
            ]
            $ minutes 9 <> seconds 54
        (expectedSessionTrace, expectedEventTrace) =
          ( [ ( Timestamp 1,
                Charging {batteryLevel = 16000.0, energyDelivered = 0.0, currentOffered = 11.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 180001)}
              ),
              ( Timestamp 60001,
                Charging {batteryLevel = 16037.47, energyDelivered = 44.08, currentOffered = 11.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 180001)}
              ),
              ( Timestamp 62001,
                Charging {batteryLevel = 16038.72, energyDelivered = 45.55, currentOffered = 10.0, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 180001)}
              ),
              ( Timestamp 122001,
                Charging {batteryLevel = 16071.30, energyDelivered = 83.89, currentOffered = 10.0, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 180001)}
              ),
              ( Timestamp 180001,
                Charging {batteryLevel = 16102.80, energyDelivered = 120.94, currentOffered = 10.0, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = Sampled (Timestamp 180000) (MeterValues {mvTransactionId = TransactionId "4321234", mvStartDateTime = Timestamp 0, mvStationId = "id_station", mvConnectorId = 2, mvTimestamp = Timestamp 180001, mvCurrents = (0.0, 10.0, 0.0), mvOfferedCurrent = 10.0, mvEnergyDelivered = 120.94})}
              ),
              ( Timestamp 180101,
                Charging {batteryLevel = 16102.86, energyDelivered = 121.01, currentOffered = 10.0, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 360001)}
              ),
              ( Timestamp 240101,
                Charging {batteryLevel = 16135.44, energyDelivered = 159.34, currentOffered = 10.0, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 360001)}
              ),
              ( Timestamp 300101,
                Charging {batteryLevel = 16168.02, energyDelivered = 197.67, currentOffered = 10.0, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 360001)}
              ),
              ( Timestamp 312001,
                Charging {batteryLevel = 16174.48, energyDelivered = 205.28, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 360001)}
              ),
              ( Timestamp 360001,
                Charging {batteryLevel = 16196.64, energyDelivered = 231.34, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = Sampled (Timestamp 360000) (MeterValues {mvTransactionId = TransactionId "4321234", mvStartDateTime = Timestamp 0, mvStationId = "id_station", mvConnectorId = 2, mvTimestamp = Timestamp 360001, mvCurrents = (0.0, 8.5, 0.0), mvOfferedCurrent = 8.5, mvEnergyDelivered = 231.34})}
              ),
              ( Timestamp 360101,
                Charging {batteryLevel = 16196.69, energyDelivered = 231.40, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 540001)}
              ),
              ( Timestamp 420101,
                Charging {batteryLevel = 16224.38, energyDelivered = 263.98, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 540001)}
              ),
              ( Timestamp 480101,
                Charging {batteryLevel = 16252.08, energyDelivered = 296.56, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 540001)}
              ),
              ( Timestamp 540001,
                Charging {batteryLevel = 16279.73, energyDelivered = 329.09, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = Sampled (Timestamp 540000) (MeterValues {mvTransactionId = TransactionId "4321234", mvStartDateTime = Timestamp 0, mvStationId = "id_station", mvConnectorId = 2, mvTimestamp = Timestamp 540001, mvCurrents = (0.0, 8.5, 0.0), mvOfferedCurrent = 8.5, mvEnergyDelivered = 329.09})}
              ),
              ( Timestamp 540101,
                Charging {batteryLevel = 16279.77, energyDelivered = 329.14, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 720000)}
              ),
              ( Timestamp 594001,
                Charging {batteryLevel = 16304.66, energyDelivered = 358.42, currentOffered = 8.5, transactionId = TransactionId "4321234", startDateTime = Timestamp 0, meterValuesStateMachine = NextMeterValueSampleDue (Timestamp 720000)}
              )
            ],
            [ (Timestamp 62001, AcceptSetChargingProfile "cb10"),
              (Timestamp 180101, SendMeterValues (MeterValues {mvTransactionId = TransactionId "4321234", mvStartDateTime = Timestamp 0, mvStationId = "id_station", mvConnectorId = 2, mvTimestamp = Timestamp 180001, mvCurrents = (0.0, 10.0, 0.0), mvOfferedCurrent = 10.0, mvEnergyDelivered = 120.94166666666698})),
              (Timestamp 312001, AcceptSetChargingProfile "cb8p5"),
              (Timestamp 360101, SendMeterValues (MeterValues {mvTransactionId = TransactionId "4321234", mvStartDateTime = Timestamp 0, mvStationId = "id_station", mvConnectorId = 2, mvTimestamp = Timestamp 360001, mvCurrents = (0.0, 8.5, 0.0), mvOfferedCurrent = 8.5, mvEnergyDelivered = 231.34166666666715})),
              (Timestamp 540101, SendMeterValues (MeterValues {mvTransactionId = TransactionId "4321234", mvStartDateTime = Timestamp 0, mvStationId = "id_station", mvConnectorId = 2, mvTimestamp = Timestamp 540001, mvCurrents = (0.0, 8.5, 0.0), mvOfferedCurrent = 8.5, mvEnergyDelivered = 329.09166666666675}))
            ]
          )
     in [ testCase "session trace length" $ assertEqual "prefix" (length sessionTrace) (length expectedSessionTrace),
          testCase "event trace length" $ assertEqual "prefix" (length eventTrace) (length expectedEventTrace)
        ]
          <> [ testCase ("session state @ " <> show expTs) $ do
                 assertEqual "Timestamp" expTs obsTs
                 assertApproxEqual "Energy delivered" 1 obsEnergyDelivered expEnergyDelivered
                 assertApproxEqual "Current offered" 1 obsCurrentOffered expCurrentOffered
                 assertApproxEqual "Battery level" 1 obsBatteryLevel expBatteryLevel
             | ( (obsTs, Session _ (Charging {energyDelivered = obsEnergyDelivered, currentOffered = obsCurrentOffered, batteryLevel = obsBatteryLevel})),
                 (expTs, Charging {energyDelivered = expEnergyDelivered, currentOffered = expCurrentOffered, batteryLevel = expBatteryLevel})
                 ) <-
                 zip sessionTrace expectedSessionTrace
             ]
          <> [ testCase ("event @ " <> show expTs) $ do
                 assertEqual "Timestamp" expTs obsTs
                 assertEqual "Event" expEvt obsEvt
             | ( (obsTs, obsEvt),
                 (expTs, expEvt)
                 ) <-
                 zip eventTrace expectedEventTrace
             ]

timeConversionFunctionTests :: TestTree
timeConversionFunctionTests =
  testGroup
    "Time conversion functions"
    [ testCase "simulationTimeToUTCTime forward" $
        simulationTimeToUTCTime (Timestamp 123) (UTCTime {utctDay = ModifiedJulianDay 666, utctDayTime = 0}) (Timestamp 86400123)
          @?= UTCTime {utctDay = ModifiedJulianDay 667, utctDayTime = 0},
      testCase "simulationTimeToUTCTime backward" $
        simulationTimeToUTCTime (Timestamp 172800789) (UTCTime {utctDay = ModifiedJulianDay 668, utctDayTime = 0}) (Timestamp 86400789)
          @?= UTCTime {utctDay = ModifiedJulianDay 667, utctDayTime = 0},
      testCase "utcTimeToSimulationTime forward" $
        utcTimeToSimulationTime (Timestamp 123) (UTCTime {utctDay = ModifiedJulianDay 666, utctDayTime = 0}) (UTCTime {utctDay = ModifiedJulianDay 667, utctDayTime = 0})
          @?= Timestamp 86400123,
      testCase
        "utcTimeToSimulationTime backward"
        $ utcTimeToSimulationTime (Timestamp 172800456) (UTCTime {utctDay = ModifiedJulianDay 666, utctDayTime = 0}) (UTCTime {utctDay = ModifiedJulianDay 665, utctDayTime = 0})
          @?= Timestamp 86400456
    ]

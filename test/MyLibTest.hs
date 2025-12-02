module Main (main) where

import ChargingStation
import Control.Monad (forM_)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

-- | round x to n digits
approx :: Int -> Double -> Double
approx n x = fromInteger (round $ x * (10 ^ n)) / (10.0 ^ n)

-- | assert approximate equality
assertApproxEqual :: String -> Int -> Double -> Double -> Assertion
assertApproxEqual prefix n observed expected =
  assertEqual prefix (approx n expected) (approx n observed)

main :: IO ()
main = defaultMain manuallyCheckedComputationTests

manuallyCheckedComputationTests :: TestTree
manuallyCheckedComputationTests =
  testGroup
    "Manually checked computation"
    [ testCase "Just before reaching 80% charge" $ do
        let SimState {tick = ts, session = Session _ sess} = states !! 1
        ts @?= 60000
        assertApproxEqual "batteryLevel" 3 (batteryLevel sess) 64000.0
        assertApproxEqual "energyDelivered" 3 (energyDelivered sess) 10122.666666,
      testCase "Just after reaching 80% charge" $ do
        let SimState {tick = ts, session = Session _ sess} = states !! 2
        ts @?= 120000
        assertApproxEqual "batteryLevel" 3 (batteryLevel sess) 64092.0
        assertApproxEqual "energyDelivered" 3 (energyDelivered sess) 10245.333333,
      testCase "No meter values before 5min" $ do
        forM_ (takeWhile (\(SimState {tick}) -> tick < meterValuesPeriodicity) states) $ \(SimState {tick, session = session@(Session _ (Charging {meterValuesStateMachine = (NextMeterValueSampleDue nextMeterValueTs)}))}) -> do
          assertEqual "Next meter values sample due" meterValuesPeriodicity nextMeterValueTs,
      testCase "Meter value sampled after 5min" $ do
        let (SimState {tick, session = session@(Session _ (Charging {meterValuesStateMachine = (Sampled sampleTimestamp sampledValues)}))} : _) = dropWhile (\(SimState {tick}) -> tick < meterValuesPeriodicity) states
        assertEqual "Sampled at" meterValuesPeriodicity sampleTimestamp
        assertApproxEqual "Energy delivered" 3 (mvEnergyDelivered sampledValues) 10609.380,
      testCase "Meter value sampled after 10min" $ do
        let (SimState {tick, session = session@(Session _ (Charging {meterValuesStateMachine = (Sampled sampleTimestamp sampledValues)}))} : _) = dropWhile (\(SimState {tick}) -> tick < 2 * meterValuesPeriodicity) states
        assertEqual "Sampled at" (2 * meterValuesPeriodicity) sampleTimestamp
        assertApproxEqual "Energy delivered" 3 (mvEnergyDelivered sampledValues) 11203.159
    ]
  where
    meterValuesPeriodicity = 5 * 60 * 1000
    sessConf =
      SessionConfiguration
        { batteryCapacity = 80000.0,
          sessionTarget = LeaveAtTick $ 60 * 20 * 1000, -- 20 min charging time
          charge = chargeEfficientlyUntil80Percent sessConf,
          instantaneousCurrent = instantaneousCurrentMaxUntil80Percent 16 0.2 sessConf,
          phases = RT,
          stationId = "station_id",
          connectorId = 1,
          meterValuesPeriodicity = meterValuesPeriodicity
        }
    stepByMinute :: SimState -> SimState
    stepByMinute simState@(SimState {tick}) = stepSimulation simState $ tick + 1000 * 60
    states :: [SimState]
    states =
      let tick = 0
       in iterate stepByMinute $
            SimState
              { tick = tick,
                session =
                  Session sessConf $
                    Charging
                      { transactionId = 12341234,
                        batteryLevel = 64000.0 - 104.266666,
                        energyDelivered = 10000.0,
                        currentOffered = 20,
                        meterValuesStateMachine = NextMeterValueSampleDue $ tick + meterValuesPeriodicity
                      }
              }

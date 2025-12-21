module Main (main) where

import ChargingStation
import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer.Strict (runWriter, runWriterT)
import Data.Text (Text)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

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
main = defaultMain manuallyCheckedComputationTests

manuallyCheckedComputationTests :: TestTree
manuallyCheckedComputationTests =
  testGroup
    "Manually checked computation"
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
          transactionId = 1234321,
          meterValuesStateMachine = NextMeterValueSampleDue $ meterValuesPeriodicity `after` startTime
        }
    stepByMinute :: SimState -> (OutputEvent [] MeterValues, SimState)
    stepByMinute simState@(SimState {tick}) = stepSimulation simState $ minutes 1 `after` tick
    states =
      snd $
        iterateWriter stepByMinute 25 $
          SimState {tick = startTime, session = Session sessConf sessState}

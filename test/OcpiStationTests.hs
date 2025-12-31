{-# LANGUAGE Trustworthy #-}

module OcpiStationTests (tests) where

import ChargingStation
  ( Duration (..),
    MeterValuesStateMachine (..),
    Phases (..),
    Session (..),
    SessionConfiguration (..),
    SessionState (..),
    SessionTarget (..),
    Timestamp (..),
    TransactionId (..),
    after,
    before,
    chargeEfficientlyUntil80Percent,
    getInstantaneousCurrentMaxUntil80Percent,
    milliseconds,
    minutes,
    seconds,
    simulationTimeToUTCTime,
    stepSession,
  )
import Control.Monad (join, unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import Data.Time (Day (..), UTCTime (..))
import Data.Word (Word8)
import FakeSimulation (fakeSimulation)
import Network.HTTP (HeaderName (..), Request (..), hdrName, hdrValue)
import Network.URI (URI (..), URIAuth (..), nullURI, nullURIAuth)
import OcpiStation
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify

tests =
  [ testProperty "parallel simulation is equivalent to separate simulation" prop_parallel_simulation_is_equivalent_to_separate_simulation
  ]

prop_parallel_simulation_is_equivalent_to_separate_simulation :: Property ()
prop_parallel_simulation_is_equivalent_to_separate_simulation = do
  -- setup
  startTime <- gen $ Timestamp <$> Gen.elem (1000000 :| [12345678])
  duration <-
    gen $
      (<>)
        <$> Gen.elem (mempty :| [milliseconds 123])
        <*> Gen.elem (mempty :| [milliseconds 1, seconds 1, minutes 1, minutes 2, minutes 10, minutes 60, minutes $ 60 * 24])
  sessionCount <- gen $ Gen.inRange $ Range.withOrigin (2, 10) 2
  sessions <- gen $ mapM (genSession startTime duration) [1 .. sessionCount]
  let config = Config {scspBaseUrl = "http://scsp.example.com/callback", scspToken = "Token ?", timestampToUtcTime = simulationTimeToUTCTime startTime $ UTCTime {utctDay = ModifiedJulianDay 666, utctDayTime = 0}}
  let separateSimulations = map (\sess -> fakeSimulation stepSession sess startTime [] duration) sessions
  let parallelSimulation = startMultiSimulation config channel multiSession startTime duration
  -- assert $ P.even `P.dot` P.fn ("multiply3", (* 3)) P..$ ("x", length sessions)
  assert
    ( P.eq
        P..$ ("length sessions", length sessions)
        P..$ ("length separateSimulations", length separateSimulations)
    )

genSession :: Timestamp -> Duration -> Word8 -> Gen Session
genSession startTime simulationDuration sessionNumber =
  Session
    <$> genSessionConfiguration sessionNumber startTime simulationDuration
    <*> genSessionState sessionNumber startTime simulationDuration

genSessionConfiguration :: Word8 -> Timestamp -> Duration -> Gen SessionConfiguration
genSessionConfiguration sessionNumber startTime simulationDuration = do
  capacity <- Gen.elem $ 40000.0 :| [50000.0, 60000.0, 70000.0]
  target <- join $ Gen.elem $ leaveAtTick :| [leaveAtTick, leaveAfterEither, leaveAfterBoth]
  maxCurrent <- Gen.elem $ 16 :| [8.5, 10]
  phases <- Gen.elem $ R :| [S, RS, RST]
  periodicity <- Gen.elem $ minutes 5 :| [minutes 1, minutes 60, seconds 53, minutes 1 <> seconds 11]
  let sessConf =
        SessionConfiguration
          { batteryCapacity = capacity,
            sessionTarget = target,
            charge = chargeEfficientlyUntil80Percent sessConf,
            getInstantaneousCurrent = getInstantaneousCurrentMaxUntil80Percent 14 0.0 sessConf,
            phases = phases,
            stationId = "SID",
            connectorId = sessionNumber,
            meterValuesPeriodicity = periodicity
          }
  return sessConf
  where
    possibleDurations :: NonEmpty Duration
    possibleDurations =
      minutes 60
        :| ( map ((seconds 29 <>) . minutes) [0, 1, 2, 4, 8, 16, 32, 64]
               ++ [simulationDuration, minutes 5 <> simulationDuration]
           )
    genTickForLeaving :: Gen Timestamp
    genTickForLeaving = Gen.elem $ (`after` startTime) <$> possibleDurations
    genLevelForLeaving :: Gen Double
    genLevelForLeaving = Gen.elem $ 1 :| [1, 1000, 2000, 20000, 25434, 30000, 40000, 100000]
    leaveAtLevel = LeaveAtLevel <$> genLevelForLeaving
    leaveAtTick = LeaveAtTick <$> genTickForLeaving
    leaveAfterBoth = LeaveAfterBoth <$> genTickForLeaving <*> genLevelForLeaving
    leaveAfterEither = LeaveAfterEither <$> genTickForLeaving <*> genLevelForLeaving

genSessionState :: Word8 -> Timestamp -> Duration -> Gen SessionState
genSessionState sessionNumber startTime simulationDuration = do
  batteryLevel <- Gen.choose (Gen.elem $ 0.0 :| [1000, 1234, 10000, 80000, 120000]) ((/ 10.0) . fromIntegral <$> Gen.int (Range.between (0, 800000)))
  energyDelivered <- Gen.elem $ 0.0 :| filter (< batteryLevel) [123.4, 1000, 10000, 80000, 120000]
  currentOffered <- Gen.elem $ 0.0 :| [6, 8, 25]
  startDateTime <- Gen.elem $ startTime :| [minutes 60 `before` startTime, (minutes 41 <> seconds 37) `before` startTime]
  delayToNextState <- seconds <$> Gen.elem (60 :| [0, 1, 17, 120, 300, 337, 10000])
  return
    Charging
      { batteryLevel = batteryLevel,
        energyDelivered = energyDelivered,
        currentOffered = currentOffered,
        transactionId = TransactionId $ "T" <> pack (show sessionNumber),
        startDateTime = startDateTime,
        meterValuesStateMachine = NextMeterValueSampleDue $ delayToNextState `after` startTime
      }

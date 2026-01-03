{-# LANGUAGE Trustworthy #-}

module OcpiStationTests (tests) where

import ChargingStation
  ( Duration (..),
    InputEvent (..),
    MeterValuesStateMachine (..),
    Phases (..),
    Session (..),
    SessionConfiguration (..),
    SessionOutput,
    SessionState (..),
    SessionTarget (..),
    SimulationSetChargingProfile (..),
    Timestamp (..),
    TransactionId (..),
    after,
    before,
    chargeEfficientlyUntil80Percent,
    clampingDurationUntil,
    getInstantaneousCurrentMaxUntil80Percent,
    milliseconds,
    minutes,
    seconds,
    simulationTimeToUTCTime,
    stepSession,
  )
import Control.Monad (join, unless)
import Control.Monad.RWS (RWS (..), get, put, runRWS, tell)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import Data.Time (Day (..), UTCTime (..))
import Data.Word (Word8)
import Debug.Trace (trace)
import FakeSimulation (fakeSimulation, safeCalendarToCalendar)
import Network.HTTP (HeaderName (..), Request (..), hdrName, hdrValue)
import Network.URI (URI (..), URIAuth (..), nullURI, nullURIAuth)
import OcpiStation
import PriorityMap qualified
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
  startTime <- gen $ Timestamp <$> Gen.elem (10000000000 :| [123456780000])
  duration <-
    gen $
      (\d1 d2 -> max (milliseconds 2) $ d1 <> d2)
        <$> Gen.elem (mempty :| [milliseconds 123])
        <*> Gen.elem (mempty :| [milliseconds 2, seconds 1, minutes 1, minutes 2, minutes 10, minutes 60, minutes $ 60 * 24])
  sessionCount <- gen $ Gen.inRange $ Range.withOrigin (2, 10) 2
  sessions <- gen $ mapM (genSession startTime duration) [1 .. sessionCount]
  calendar <- gen $ genSafeCalendar sessionCount duration
  let separateSimulations = map (\sess -> fakeSimulation stepSession sess startTime calendar duration) sessions
  let ((), remainingCalendar, parallelSimulationTrace) =
        ( runRWS $
            startMultiSimulation
              stepSession
              fakeDeliverSessionOutput
              fakeGetNextEvent
              (\ms scp -> let r = mapEventToSession ms scp in trace ("mapEventToSession " <> show ms <> " " <> show scp <> " -> " <> show r) r)
              fakeGetSimTime
              fakeLogString
              (PriorityMap.fromListContents $ map (makePKV startTime) sessions)
              startTime
              duration
        )
          ()
          $ safeCalendarToCalendar startTime (duration `after` startTime) calendar
  -- assert $ P.even `P.dot` P.fn ("multiply3", (* 3)) P..$ ("x", length sessions)
  assert (P.eq P..$ ("remainingCalendar", remainingCalendar) P..$ ("empty list", []))
  assert (P.eq
          P..$ ("parallelSimulationTrace", keepRights parallelSimulationTrace)
          P..$ ("separate simulation trace", map snd $ snd $ head separateSimulations))
  assert
    ( P.eq
        P..$ ("length sessions", length sessions)
        P..$ ("length separateSimulations", length separateSimulations)
    )
  where
    makePKV startTime sess =
      (milliseconds 1 `after` startTime, sessionKey sess, (sess, startTime))

genSafeCalendar :: Word8 -> Duration -> Gen [(Duration, SimulationSetChargingProfile)]
genSafeCalendar sessionCount totalDuration = go (seconds 0) makeNonTrivial []
  where
    go ::
      Duration ->
      ([(Duration, SimulationSetChargingProfile)] -> [(Duration, SimulationSetChargingProfile)]) ->
      [(Duration, SimulationSetChargingProfile)] ->
      Gen [(Duration, SimulationSetChargingProfile)]
    go totalDurationSoFar calendar calTail = do
      nextDuration <- milliseconds <$> Gen.inRange (Range.withOrigin (1, 120000) 100)
      let nextTotal = totalDurationSoFar <> nextDuration
      if nextTotal >= totalDurationSoFar
        then return $ calendar calTail
        else do
          -- also generate invalid external inputs
          sessionNumber <- makeInvalidSessionNumberRecognizable <$> Gen.inRange (Range.between (0, sessionCount + 1))
          let setChargingProfile =
                SimulationSetChargingProfile
                  { scpTransactionId = sessionTransactionId sessionNumber,
                    scpCurrentOffered = 6.0,
                    scpCallback = pack $ "cb" <> show nextTotal
                  }
          go
            nextTotal
            (calendar . ((nextDuration, setChargingProfile) :))
            calTail
    makeNonTrivial :: [(Duration, SimulationSetChargingProfile)] -> [(Duration, SimulationSetChargingProfile)]
    makeNonTrivial [] =
      [ ( milliseconds 1,
          SimulationSetChargingProfile
            { scpTransactionId = sessionTransactionId 0,
              scpCurrentOffered = 7.0,
              scpCallback = "nontrivial"
            }
        )
      ]
    makeNonTrivial x = x
    makeInvalidSessionNumberRecognizable x
      -- make invalid external inputs easier to recognize by having a clearly different TransactionId
      | x >= sessionCount = maxBound
      | otherwise = x

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
        transactionId = sessionTransactionId sessionNumber,
        startDateTime = startDateTime,
        meterValuesStateMachine = NextMeterValueSampleDue $ delayToNextState `after` startTime
      }

sessionTransactionId :: Word8 -> TransactionId
sessionTransactionId sessNo = TransactionId $ "T" <> pack (show sessNo)

type FakeMultiSimulation = RWS () [Either String SessionOutput] [InputEvent SimulationSetChargingProfile]

fakeDeliverSessionOutput :: SessionOutput -> FakeMultiSimulation ()
fakeDeliverSessionOutput = tell . (: []) . Right

fakeGetNextEvent :: Timestamp -> Duration -> FakeMultiSimulation (Maybe (Duration, SimulationSetChargingProfile))
fakeGetNextEvent deadline duration = do
  calendar <- get
  case calendar of
    [] -> trace ("fakeGetNextEvent " <> show deadline <> " " <> show duration <> ": empty calendar") $ return Nothing
    InputEvent {ieTick, ieTrigger} : tl
      | ieTick > deadline -> trace ("fakeGetNextEvent " <> show deadline <> " " <> show duration <> ": no external event") $ return Nothing
      | otherwise -> do
          trace ("fakeGetNextEvent " <> show deadline <> " " <> show duration <> ": " <> show ieTrigger <> "@" <> show ieTick <> " " <> show (length tl) <> " events left") $ put tl
          return $ fmap (\trggr -> (ieTick `clampingDurationUntil` deadline, trggr)) ieTrigger

fakeGetSimTime :: MultiSession -> FakeMultiSimulation Timestamp
fakeGetSimTime allSessions =
  case PriorityMap.lookupFirst allSessions of
    Just (_, _, (_, ts)) -> return ts
    Nothing -> error "fakeSimulation can't run without any Session"

fakeLogString :: String -> FakeMultiSimulation ()
fakeLogString = tell . (: []) . Left

keepRights :: [Either a b] -> [b]
keepRights = foldr keepRights []
  where keepRights (Right b) = (b:)
        keepRights (Left _) = id

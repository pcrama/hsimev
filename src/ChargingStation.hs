module ChargingStation (
  stepSimulation
  , SessionTarget(..)
  , Timestamp
  , Duration
  , Phases(..)
  , SessionState(..)
  , SessionConfiguration(..)
  , Session(..)
  , SimState(..)
  , chargeEfficientlyUntil80
  , instantaneousCurrentMaxUntil80Percent
  , tagada) where

import Data.Word (Word64)

type Timestamp = Word64 -- [ms] since some imaginary epoch

type Duration = Word64 -- [ms]

data SessionTarget = LeaveAtTick Timestamp -- driver wants to leave at a fixed time
                   | LeaveAfterBoth Timestamp Double -- driver wants to leave at the earliest after the given time but only if their charge level is sufficient
                   | LeaveAfterEither Timestamp Double -- driver will stay until the given time or as soon as their battery has reached that charge level
                   | LeaveAtLevel Double -- drive will stay until their battery has reached that charge level
  deriving (Show, Eq)

data Phases = R -- Mono on 1st phase
            | S -- Mono on 2nd phase
            | T -- Mono on 3rd phase
            | RS -- Two phases
            | ST -- Two phases
            | RT -- Two phases
            | RST -- Three phases
  deriving (Show, Eq)

data SessionState = SessionState {
  batteryLevel :: Double -- how many [Wh] are in the car's battery
  , currentOffered :: Double -- how many [A] the EV should use
  } deriving (Show, Eq)

data SessionConfiguration = SessionConfiguration {
  batteryCapacity :: Double -- how many [Wh] "fit" in the car's battery
  , sessionTarget :: SessionTarget
  , charge :: Timestamp -> SessionState -> Duration -> Maybe SessionState
  , instantaneousCurrent :: Timestamp -> SessionState -> (Double, Double, Double)
  , phases :: Phases
  }

data Session = Session SessionConfiguration SessionState

data SimState = SimState {
  tick :: Timestamp
  , session :: Maybe Session
  }

countPhases :: (Double, Double, Double) -> Int
countPhases (x, y, z) = (if x > 0.0 then 1 else 0) + (if y > 0.0 then 1 else 0) + (if z > 0.0 then 1 else 0)

chargeEfficientlyUntil80 :: SessionConfiguration -> Timestamp -> SessionState -> Duration -> Maybe SessionState
chargeEfficientlyUntil80 (SessionConfiguration { instantaneousCurrent, sessionTarget, batteryCapacity }) t0 sessionState@(SessionState { batteryLevel, currentOffered }) dt =
  case sessionTarget of
    LeaveAtTick ts | ts <= t0 + dt -> Nothing
    LeaveAfterBoth ts targetLevel | (ts <= t0 + dt) && (targetLevel <= newLevel) -> Nothing
    LeaveAfterEither ts targetLevel | (ts <= t0 + dt) || (targetLevel <= newLevel) -> Nothing
    LeaveAtLevel targetLevel | targetLevel <= newLevel -> Nothing
    _ -> Just $ sessionState { batteryLevel=newLevel }
  where newLevel = batteryLevel + (sumOfCurrents * chargingEfficiencyAsVoltage * fromIntegral dt) / 3600.0 / 1000.0
        batteryPercentage = batteryLevel / batteryCapacity
        chargingEfficiencyAsVoltage = 230 * (if batteryPercentage < 0.8 then 0.85 else 0.75)
        sumOfCurrents = let (x, y, z) = instantaneousCurrent t0 sessionState in x + y + z

instantaneousCurrentMaxUntil80Percent :: Double -> Double -> SessionConfiguration -> Timestamp -> SessionState -> (Double, Double, Double)
instantaneousCurrentMaxUntil80Percent
  maxCurrent
  offset
  (SessionConfiguration { batteryCapacity, phases })
  _ -- timestamp, ignored
  (SessionState { batteryLevel, currentOffered }) =
  case phases of R -> (cappedCurrent, 0.0, 0.0)
                 S -> (0.0, cappedCurrent, 0.0)
                 T -> (0.0, 0.0, cappedCurrent)
                 RS -> (cappedCurrent, cappedCurrent, 0.0)
                 ST -> (0.0, cappedCurrent, cappedCurrent)
                 RT -> (cappedCurrent, 0.0, cappedCurrent)
                 RST -> (cappedCurrent, cappedCurrent, cappedCurrent)
  where batteryPercentage = batteryLevel / batteryCapacity
        knee = 0.8
        batteryCurrent = if batteryPercentage < knee then maxCurrent else 1.0 + (maxCurrent - 1.0) * (1.0 - batteryPercentage) / (1 - knee)
        cappedCurrent = max 0.1 (min (currentOffered - offset) batteryCurrent)

stepSimulation :: SimState -> Timestamp -> SimState
stepSimulation (SimState{ session=Nothing }) ts = SimState { tick=ts, session=Nothing }
stepSimulation (SimState{ tick=t0, session=Just (Session sessionConfiguration sessionState) }) ts | ts > t0 = SimState { tick=ts, session = Session sessionConfiguration <$> charge sessionConfiguration t0 sessionState (ts - t0) }
stepSimulation _ _ = error "Can't simulate time backwards"

sessConf = SessionConfiguration {
  batteryCapacity = 80000.0
  , sessionTarget = LeaveAtTick $ 60 * 20 * 1000 -- 20 min charging time
  , charge = chargeEfficientlyUntil80 sessConf
  , instantaneousCurrent = instantaneousCurrentMaxUntil80Percent 16 0.2 sessConf
  , phases = RT
  }

stepByMinute :: SimState -> SimState
stepByMinute simState@(SimState { tick }) = stepSimulation simState $ tick + 1000 * 60

tagada :: [SimState]
tagada = iterate stepByMinute $ SimState { tick=0, session=Just $ Session sessConf $ SessionState { batteryLevel=70000.0, currentOffered=20 }}

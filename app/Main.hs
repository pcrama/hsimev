module Main where

import Control.Monad (forM_)
import qualified MyLib (someFunc)
import qualified ChargingStation (tagada, SimState(..), Session(..))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  forM_ (take 25 ChargingStation.tagada) $ \simState@(ChargingStation.SimState { ChargingStation.tick, ChargingStation.session }) ->
    case session of
      Nothing -> putStrLn $ show tick <> ": Nothing"
      Just (ChargingStation.Session _ sessionState) -> putStrLn $ show tick <> ": " <> show sessionState
  MyLib.someFunc

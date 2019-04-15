{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import Control.Lens
import Data.Time
import Data.Time.Calendar

type Balance = Double
type PlotData = [(LocalTime, Balance)]
type ProjectionData = (LocalTime, Balance)


data Contribution = Contribution
  { _frequency :: Integer -- days
  , _amount    :: Balance
  }
makeLenses ''Contribution

-- 
-- projectDate startingBalance startingDate (Contribution f a) endBalance =
--   endDate
--   where
--     endDate = 

-- projectBalance :: Balance -> LocalTime -> Contribution -> LocalTime -> Balance
-- projectBalance startingBalance startingDate (Contribution f a) endDate = endBalance
--   where
--     endBalance = 

-- stepsFromTo :: 
mkProjectionData :: Day -> Balance -> ProjectionData
mkProjectionData d b = ((LocalTime d midnight), b)
  
projectDate :: Balance -> Day -> Contribution -> Balance -> [ProjectionData]
projectDate startingBalance startingDate (Contribution f increment) expectedBalance =
  zipWith (flip mkProjectionData) balanceSteps allDates
  where
    balanceSteps = takeWhile (<=expectedBalance) allBalances
    allBalances  = iterate (+increment) startingBalance
    allDates     = iterate (addDays f)  startingDate

projectBalance :: Balance -> Day -> Contribution -> Day -> [ProjectionData]
projectBalance startingBalance startingDate (Contribution f increment) expectedDate =
  zipWith mkProjectionData dateSteps allBalances
  where
    dateSteps    = takeWhile (<=expectedDate) allDates
    allBalances  = iterate (+increment) startingBalance
    allDates     = iterate (addDays f)  startingDate

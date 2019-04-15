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

projectDate :: Balance -> Day -> Contribution -> Balance -> [(Balance, Day)]
projectDate startingBalance startingDate (Contribution f increment) expectedBalance =
  zip balanceSteps allDates
  where
    balanceSteps = takeWhile (<=expectedBalance) allBalances
    allBalances  = iterate (+increment) startingBalance
    allDates     = iterate (addDays f)  startingDate

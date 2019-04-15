{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import Control.Lens
import Data.Time
import Data.Time.Calendar

type Balance = Double
type PlotData = [(Day, Balance)]
type ProjectionData = (Day, Balance)


data Contribution = Contribution
  { _frequency :: Integer -- days
  , _amount    :: Balance
  } deriving Show
makeLenses ''Contribution


-- small utility fns:

balancesUntil expectedBalance = takeWhile (<=expectedBalance)
stepBalances  step            = iterate (+step)
datesUntil    expectedDate    = takeWhile (<=expectedDate)
stepDates     step            = iterate (addDays step)



projectDate :: ProjectionData -> Contribution -> Balance -> (Day, [ProjectionData])
projectDate (startingDate, startingBalance) (Contribution f increment) expectedBalance =
  (endDate, datesToBal)
  where
    (endDate, _) = last datesToBal
    datesToBal   = zipWith (flip (,)) balanceSteps allDates
    balanceSteps = takeWhile (<=expectedBalance) allBalances
    allBalances  = iterate (+increment) startingBalance
    allDates     = iterate (addDays f)  startingDate

projectBalance :: ProjectionData -> Contribution -> Day -> (Balance, [ProjectionData])
projectBalance (startingDate, startingBalance) (Contribution f increment) expectedDate =
  (endBalance, balsToDate)
  where
    (_, endBalance) = last balsToDate
    balsToDate      = zipWith (,) dateSteps allBalances
    dateSteps    = takeWhile (<=expectedDate) allDates
    allBalances  = iterate (+increment) startingBalance
    allDates     = iterate (addDays f)  startingDate

-- projectContribution :: ProjectionData -> Day -> Balance -> (Maybe Int, Maybe Balance) -> (Contribution, [ProjectionData])
-- projectContribution (sd, sb) ed eb (mFreq, mBal) = case (mFreq, mBal) of
--   (Just freq,  Nothing) -> guessContributionAmount (sd, sb) ed eb freq
--   (Nothing,  Just bal ) -> guessContributionFreq   (sd, sb) ed eb bal
--   (Just freq, Just bal) -> expandContribution      (sd, sb) ed eb (Contribution freq bal)


guessContributionAmount :: ProjectionData -> Day -> Balance -> Integer -> (Contribution, [ProjectionData])
guessContributionAmount (startingDay, startingBalance) endDay endBalance frequency =
  ((Contribution frequency increment), steps)
  where
    steps        = zip dateSteps balanceSteps
    balanceSteps = takeWhile (<=endBalance) allBalances
    dateSteps    = takeWhile (<=endDay) allDates
    allBalances  = iterate (+increment) startingBalance
    allDates     = iterate (addDays frequency) startingDay
    increment    = (endBalance - startingBalance) / fromIntegral (length dateSteps)

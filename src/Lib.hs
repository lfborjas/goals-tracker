{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import Control.Lens
import Data.Time
import Data.Time.Calendar

type Balance = Double
type PlotData = [(Day, Balance)]
type ProjectionData = (Day, Balance)


data Rate = Rate
  { percent :: Double
  , payoffFrequency :: Integer
  } deriving Show

data Contribution = Contribution
  { _frequency :: Integer -- days
  , _amount    :: Balance
  } deriving Show
makeLenses ''Contribution


-- small utility fns:
-- just letting type inference do its thing with this one
allSteps incf start step =
  iterate (incf step) start

stepsUntil incf start end step
  | start < end  = stepWhen (<=end)
  | start > end  = stepWhen (>=end)
  | start == end = [start]
  where stepWhen goalSection =
          takeWhile goalSection $ allSteps incf start step

balancesUntil = stepsUntil (+)
datesUntil    = stepsUntil addDays
allBalances   = allSteps (+)
allDates      = allSteps addDays


projectDate :: ProjectionData -> Contribution -> Balance -> (Day, [ProjectionData])
projectDate (startingDate, startingBalance) (Contribution f increment) expectedBalance =
  (endDate, datesToBal)
  where
    (endDate, _) = last datesToBal
    datesToBal   = zipWith (flip (,)) balanceSteps $ allDates startingDate f
    balanceSteps = balancesUntil startingBalance expectedBalance increment


projectBalance :: ProjectionData -> Contribution -> Day -> (Balance, [ProjectionData])
projectBalance (startingDate, startingBalance) (Contribution f increment) expectedDate =
  (endBalance, balsToDate)
  where
    (_, endBalance) = last balsToDate
    balsToDate      = zipWith (,) dateSteps $ allBalances startingBalance increment
    dateSteps       = datesUntil startingDate expectedDate f


projectContribution :: ProjectionData -> Day -> Balance -> (Maybe Integer, Maybe Balance) -> (Contribution, [ProjectionData])
projectContribution (sd, sb) ed eb (mFreq, mBal) = case (mFreq, mBal) of
  (Just freq,  Nothing) -> guessContributionAmount      (sd, sb) ed eb freq
  (Nothing,  Just bal ) -> guessContributionFrequency   (sd, sb) ed eb bal
  (Just freq, Just bal) -> expandContribution           (sd, sb) ed eb (Contribution freq bal)
  (Nothing,    Nothing) -> (Contribution 0 0.0, [])


guessContributionAmount :: ProjectionData -> Day -> Balance -> Integer -> (Contribution, [ProjectionData])
guessContributionAmount (startingDay, startingBalance) endDay endBalance frequency =
  ((Contribution frequency increment), steps)
  where
    steps        = zip dateSteps balanceSteps
    balanceSteps = balancesUntil startingBalance endBalance increment
    dateSteps    = datesUntil startingDay endDay frequency
    increment    = (endBalance - startingBalance) / ((fromIntegral (length dateSteps)) - 1)

guessContributionFrequency :: ProjectionData -> Day -> Balance -> Double -> (Contribution, [ProjectionData])
guessContributionFrequency (startingDay, startingBalance) endDay endBalance amount =
  ((Contribution dateIncrement amount), steps)
  where
    steps         = zip dateSteps balanceSteps
    balanceSteps  = balancesUntil startingBalance endBalance amount
    dateSteps     = datesUntil startingDay endDay dateIncrement
    dateIncrement = (diffDays endDay startingDay) `div` (fromIntegral (length balanceSteps)) :: Integer

expandContribution :: ProjectionData -> Day -> Balance -> Contribution -> (Contribution, [ProjectionData])
expandContribution (sd, sb) ed eb c@(Contribution frequency increment) =
  (c, steps)
  where
    steps = zip dates bals
    dates = datesUntil sd ed frequency
    bals  = balancesUntil sb eb increment


calculateRunway :: Day -> Balance -> Balance -> (Day, [ProjectionData])
calculateRunway start balance expenses = projectDate (start, balance) (Contribution 30 (negate expenses)) 0

growth ::
  Integer -> -- payoff frequency
  Double  -> -- interest rate
  Double  -> -- principal
  Double     -- principal with applied interest
growth n i p = p + p * ((1+i)^n -1)

compoundingRate ::
  Rate ->    -- nominal rate, and payoff frequency
  Double     -- rate per compounding time
compoundingRate (Rate r f) = r/fromIntegral f

compoundInterest ::
  Balance ->
  Rate ->
  Day ->
  Day -> (Balance, [ProjectionData])
compoundInterest principal r@(Rate i f) start end =
  (finalBalance, steps)
  where
    steps        = zip dates bals
    finalBalance = (snd . last) steps
    dates        = datesUntil start end freq -- TODO: freq shouldn't be an int!
    freq         = 365 -- days
    rate         = (compoundingRate r)/(fromIntegral 100)
    bals         = iterate (growth f rate) principal

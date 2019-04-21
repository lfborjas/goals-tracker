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

data Frequency = Daily | Monthly | Yearly deriving Show

-- frequency in the context of a calendar year
instance Enum Frequency where
  fromEnum Daily   = 365
  fromEnum Monthly = 12
  fromEnum Yearly  = 1

  toEnum   1   = Yearly
  toEnum   12  = Monthly
  toEnum   365 = Daily
  toEnum   _   = error "Invalid frequency"

data Rate = Rate
  { percent :: Double
  , payoffFrequency :: Frequency
  } deriving Show

data Contribution_ = Contribution_
  { 
    amount_    :: Balance
  , frequency_ :: Frequency
  } deriving Show

dateInc ::
  Frequency ->
  (Integer -> Day -> Day) -- step, start, result
dateInc freq = case freq of
                 Daily   -> addDays
                 Monthly -> addGregorianMonthsClip
                 Yearly  -> addGregorianYearsClip


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
datesUntil     = stepsUntil $ dateInc Daily
allBalances   = allSteps (+)
allDates       = allSteps $ dateInc Daily

datesUntil' f = stepsUntil (dateInc f)


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

-- fancier operations

growth ::
  Rate    -> -- interest rate
  Double  -> -- principal
  Double     -- principal with applied interest
growth r@(Rate _ f) p = p + p * ((1+i)^n -1)
  where
    n  = fromEnum f
    i  = compoundingRate r

-- assumes that the contribution is made at the same frequency
-- that the interest is paid
-- TODO: fix that!
growthWithContribution c r p = c + (growth r p)

compoundingRate ::
  Rate ->    -- nominal rate, and payoff frequency
  Double     -- rate per compounding time
compoundingRate (Rate r f) = (r/fromIntegral (fromEnum f))/(fromIntegral 100)

-- this was surprisingly helpful in confirming results:
-- http://www.webmath.com/compinterest.html
compoundInterest ::
  Balance   -> -- principal
  Rate      -> -- interest rate
  Day       -> -- start date
  Day       -> -- end date
  Frequency -> -- date step
  (Balance, [ProjectionData])
compoundInterest principal rate start end freq =
  (finalBalance, steps)
  where
    steps        = zip dates bals
    finalBalance = (snd . last) steps
    dates        = datesUntil' freq start end 1
    bals         = iterate (growth rate) principal

compoundWithContribution ::
  Balance ->    -- principal today
  Rate    ->    -- interest rate
  Day -> Day -> -- start, end dats
  Contribution_  ->
  (Balance, [ProjectionData])
compoundWithContribution principal rate start end c@(Contribution_ contribution freq) =
  (finalBalance, steps)
  where
    steps = zip dates bals
    finalBalance = (snd . last) steps
    dates = datesUntil' freq start end 1
    bals = iterate (growthWithContribution contribution rate) principal

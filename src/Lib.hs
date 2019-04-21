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


-- assumes that the contribution is done at the same frequency as the
-- rate is paid off
growPrincipal ::
  Balance -> Rate -> Contribution_ -> [Balance]
growPrincipal principal rate (Contribution_ contribution _) =
  iterate (growthWithContribution contribution rate) principal

combineBalances ::
  [[Balance]] ->
  [Balance]
combineBalances projections =
  foldr1 (zipWith (+)) projections

-- NB: this is weirdly buggy -- only really works if the frequency
-- is the same as the payoff of the contribution; there must be a way
-- to generalize it??
growthInPeriod ::
  [[Balance]] -> -- all accounts involved
  Day -> Day  -> -- start, end
  Frequency   -> -- date step
  [ProjectionData]
growthInPeriod balanceHistories start end freq=
  zip dates combinedHistory
  where
    combinedHistory = combineBalances balanceHistories
    dates           = datesUntil' freq start end 1

{-
For example:

λ> b = growPrincipal 10000 (Rate 2 Monthly) (Contribution_ 1200 Monthly)
λ> a = growPrincipal 10000 (Rate 2 Monthly) (Contribution_ 1200 Monthly)
λ> growthInPeriod [a,b] (fromGregorian 2019 04 21) (fromGregorian 2020 04 21) Yearly
[(2019-04-21,20000.0),(2020-04-21,22803.68711363004)]
λ> growthInPeriod [a,b] (fromGregorian 2019 04 21) (fromGregorian 2020 04 21) Monthly
[(2019-04-21,20000.0),(2019-05-21,22803.68711363004),(2019-06-21,25663.96484518123),(2019-07-21,28581.975439813934),(2019-08-21,31558.884198171094),(2019-09-21,34595.87994173831),(2019-10-21,37694.1754875969),(2019-11-21,40855.00813275951),(2019-12-21,44079.64014828179),(2020-01-21,47369.35928334732),(2020-02-21,50725.479279527266),(2020-03-21,54149.3403954199),(2020-04-21,57642.30994187978)]
λ> 

-}

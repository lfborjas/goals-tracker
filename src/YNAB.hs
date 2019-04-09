{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module YNAB ( buildRequest
            , budgetsRequest
--            , allBudgets
            ) where

{-
Module to deal with the YNAB API:

https://api.youneedabudget.com/

-}

import Data.Aeson
import Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import GHC.Generics

accessToken :: BC.ByteString
accessToken = ""

host :: BC.ByteString
host = "api.youneedabudget.com"

budgetsPath :: BC.ByteString
budgetsPath = "/v1/budgets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "Authorization" [mconcat ["Bearer ", token]]
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

budgetsRequest :: Request
budgetsRequest = buildRequest accessToken host "GET" budgetsPath

{-
Example response:

Notice that we're using the monadic assignment (<-) since
the response is actually in an IO context; we could have also used
getResponseBody <$> response


λ> response <- httpLBS budgetsRequest
λ> getResponseBody response
"{\"data\":{\"budgets\":[{\"id\":\"EGID\",\"name\":\"EG\",\"last_modified_on\":\"2019-04-02T02:21:41+00:00\",\"first_month\":\"2019-03-01\",\"last_month\":\"2019-04-01\",\"date_format\":{\"format\":\"MM/DD/YYYY\"},\"currency_format\":{\"iso_code\":\"USD\",\"example_format\":\"123,456.78\",\"decimal_digits\":2,\"decimal_separator\":\".\",\"symbol_first\":true,\"group_separator\":\",\",\"currency_symbol\":\"$\",\"display_symbol\":true}}]}}"

-}

data YNABResponse = YNABResponse
  { responseData :: YNABData
  } deriving (Show, Generic)

-- we wrote our own parseJSON to be able to map data -> responseData
-- since `data` isn't an available name for our record (haskell keyword)
instance FromJSON YNABResponse where
  parseJSON (Object v) =
    YNABResponse <$> v .: "data"
    -- ignore the rest of the metadata for now

data YNABData = BudgetSet { budgets :: [Budget]}
  deriving (Show, Generic)

instance FromJSON YNABData -- no need to write our own here

data Budget = Budget
  { budgetId       :: T.Text
  , name           :: T.Text
  , lastModifiedOn :: T.Text
  , firstMonth     :: T.Text
  , lastMonth      :: T.Text
  } deriving (Show, Generic)

instance FromJSON Budget where
  parseJSON (Object v) =
    Budget
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "last_modified_on"
    <*> v .: "first_month"
    <*> v .: "last_month"

{-

And here is where I got stuck: there's no escaping the IO monad!
I was trying to do things in separate functions but httpLBS returns an
IO (Response LB.ByteString) and once you get in that monad, you either
go deeper or go nowhere (hence the final type signature)

-}

allBudgets :: IO (Maybe YNABResponse)
allBudgets = do
  response <- httpLBS budgetsRequest
  return $ decode $ getResponseBody response

{-

The above, in a ghci session, shows:

λ> allBudgets
Just (YNABResponse {responseData = BudgetSet {budgets = [Budget {budgetId = "EGID", name = "EG", lastModifiedOn = "2019-04-02T02:21:41+00:00", firstMonth = "2019-03-01", lastMonth = "2019-04-01"}]}})
λ> :t allBudgets
allBudgets :: IO (Maybe YNABResponse)
λ> 

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module YNAB ( allAccounts
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

accountsPath :: BC.ByteString
accountsPath = "/v1/budgets/last-used/accounts"

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

accountsRequest :: Request
accountsRequest = buildRequest accessToken host "GET" accountsPath

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
  } deriving (Show, Generic) -- TODO: need a constructor for errors, too

-- we wrote our own parseJSON to be able to map data -> responseData
-- since `data` isn't an available name for our record (haskell keyword)
instance FromJSON YNABResponse where
  parseJSON (Object v) =
    YNABResponse <$> v .: "data"
    -- ignore the rest of the metadata for now

data YNABData = AccountSet { accounts :: [Account] }
  deriving (Show, Generic)

instance FromJSON YNABData -- no need to write our own here

data Account = Account
  { accountId   :: T.Text
  , accountName :: T.Text
  , accountType :: T.Text
  , balance     :: Int
  } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON (Object v) =
    Account
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "type"
    <*> v .: "balance" -- TODO: turn into a Double? (right now this is a "millis": a 1000th of a dollar

{-

And here is where I got stuck: there's no escaping the IO monad!
I was trying to do things in separate functions but httpLBS returns an
IO (Response LB.ByteString) and once you get in that monad, you either
go deeper or go nowhere (hence the final type signature)

-}

allAccounts :: IO (Maybe YNABResponse)
allAccounts = do
  response <- httpLBS accountsRequest
  return $ decode $ getResponseBody response

{-

The above, in a ghci session, shows:

λ> allBudgets
Just (YNABResponse {responseData = BudgetSet {budgets = [Budget {budgetId = "EGID", name = "EG", lastModifiedOn = "2019-04-02T02:21:41+00:00", firstMonth = "2019-03-01", lastMonth = "2019-04-01"}]}})
λ> :t allBudgets
allBudgets :: IO (Maybe YNABResponse)
λ> 

-}

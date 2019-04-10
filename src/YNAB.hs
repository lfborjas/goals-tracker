{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module YNAB where

{-
Module to deal with the YNAB API:

https://api.youneedabudget.com/

-}

import Data.Aeson
import Data.Aeson.Lens
import Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.Wreq
import GHC.Generics
import GHC.Base
import Control.Lens
import Control.Monad

accessToken :: BC.ByteString


apiBase :: String
apiBase = "https://api.youneedabudget.com/v1"

{-
Example response for budgets:

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

data YNABData = BudgetSet  { budgets :: [Budget]}
              | AccountSet { accounts :: [Account]}
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

fromApi :: String -> IO (Response LC.ByteString)
fromApi resource = do
  let opts = defaults & auth ?~ oauth2Bearer accessToken
  r <- getWith opts $ mconcat [apiBase, resource]
  return r

{-
we can do some funky lens magic with the above method if we e.g. called the /budgets endpoint:

λ> r ^? responseBody . key "data" . key "budgets" . _Array . traverse . key "id" . _String
Just "50855054-efc4-4d49-8918-97bf198666"

λ> r <- fromApi "/budgets/last-used/accounts"
λ> r ^? responseBody . key "data" . key "accounts" . _Array

-}


-- this function feels like a bit of a defeat: why do I get to go so deeply in the lens and then have to re-get it as JSON?

{-
This works in the command line, but not as a motherfucking function:

λ> r <- fromApi "/budgets/last-used/accounts"
λ> y = decode <$> (encode <$> (r ^.. responseBody . key "data" . key "accounts" . _Array . traverse . _Object)) :: [Maybe Account]
λ> :t y
y :: [Maybe Account]

How the _fuck_ can I actually work with Accounts?????? It seems i'm trapped in the IO monad because r <- is the first
thing I do with a monad?

-}

dataPoints :: (Monad m, Functor f) => m (f Account) -> m (f Text, f Int)
dataPoints accounts = do
  account <- accounts
  -- these values can be stuck in a Maybe, so use fmap on them:
  let n = accountName <$> account
  let b = balance <$> account
  return (n, b)


isAsset :: Maybe Account -> Bool
isAsset Nothing = False
isAsset (Just a) = (accountType a) == "otherAsset"


-- notice that trackingAccounts doesn't work with a generic Functor; mostly because I couldn't for the life of me
-- extricate a Boolean out of that mess.
trackingAccounts :: (Monad m, Alternative m) => m (Maybe Account) -> m (Maybe Account)
trackingAccounts accounts = do
  account <- accounts
  let isT = isAsset account
  guard isT
  return account

-- function that takes another function to operate on all accounts
-- notice that the other function is required to deal with the fact that
-- the accounts may be Nothing
allAccounts :: ([Maybe Account] -> b) -> IO b
allAccounts g = do
  r <- fromApi "/budgets/last-used/accounts"
  let x = decode <$> (encode <$> (r ^.. responseBody . key "data" . key "accounts" . _Array . traverse . _Object)) :: [Maybe Account]
  return $ g x -- put back in the IO monad

{-
Notice that now our relevant data points are trapped in an IO monad, but we can, e.g. print with:

(allAccounts dataPoints) >>= print -- returns a list of name,balance pairs, in an IO monad
(allAccounts trackingAccounts) >>= print -- returns a list of only the tracking accounts, in IO

and potentially, plot??

-}

  

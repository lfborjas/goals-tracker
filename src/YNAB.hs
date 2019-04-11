{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module YNAB where


{-
Module to deal with the YNAB API:

https://api.youneedabudget.com/
https://api.youneedabudget.com/#endpoints
https://api.youneedabudget.com/v1#/Accounts/getAccountById

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

data Account = Account
  { accountId   :: T.Text
  , accountName :: T.Text
  , accountType :: T.Text
  , balance     :: Double
  } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON = withObject "account" $ \o -> do
    aID    <- o .: "id"
    name   <- o .: "name"
    aType  <- o .: "type"
    millis <- o .: "balance"
    let balance = millis/1000 -- YNAB returns a 10s of cents representation of money for some reason (precision?)
    return $ Account aID name aType balance



{-

And here is where I got stuck: there's no escaping the IO monad!
I was trying to do things in separate functions but httpLBS returns an
IO (Response LB.ByteString) and once you get in that monad, you either
go deeper or go nowhere (hence the final type signature)

-}

fromApi :: BC.ByteString -> String -> IO (Response LC.ByteString)
fromApi accessToken resource = do
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




{-
This works in the command line, but not as a motherfucking function:

λ> r <- fromApi "/budgets/last-used/accounts"
λ> y = decode <$> (encode <$> (r ^.. responseBody . key "data" . key "accounts" . _Array . traverse . _Object)) :: [Maybe Account]
λ> :t y
y :: [Maybe Account]

How the _fuck_ can I actually work with Accounts?????? It seems i'm trapped in the IO monad because r <- is the first
thing I do with a monad?

-}

dataPoints :: (Monad m, Functor f) => m (f Account) -> m (f Text, f Double)
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
allAccounts :: BC.ByteString -> ([Maybe Account] -> b) -> IO b
allAccounts token g = do
  r <- fromApi token "/budgets/last-used/accounts"
  let x = decode <$> (encode <$> (r ^.. responseBody . key "data" . key "accounts" . _Array . traverse . _Object)) :: [Maybe Account]
  return $ g x -- put back in the IO monad

{-
Notice that now our relevant data points are trapped in an IO monad, but we can, e.g. print with:

(allAccounts dataPoints) >>= print -- returns a list of name,balance pairs, in an IO monad
(allAccounts trackingAccounts) >>= print -- returns a list of only the tracking accounts, in IO

and potentially, plot??

Finally, all the crazy references I had to look at to finally sorta grok Aeson, Lenses and Wreq:

* https://artyom.me/aeson
* http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#g:2
* http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Prism.html
* http://deliberate-software.com/haskell-is-the-dark-souls-of-programming/
* https://www.reddit.com/r/haskell/comments/23q8kc/wreq_a_capable_new_http_client_library/
* http://hao.codes/lenses-heart-json.html
* https://conscientiousprogrammer.com/blog/2015/12/04/24-days-of-hackage-2015-day-4-wreq-web-client-programming-with-notes-on-lens-and-operator-syntax/
* http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Getter.html
* http://hackage.haskell.org/package/lens-4.15.3/docs/Control-Lens-Getter.html#v:view
* http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Fold.html
* https://codereview.stackexchange.com/questions/115066/using-wreq-and-lens-libraries-to-query-prosper-for-account-info
* https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
* https://github.com/ekmett/lens/blob/master/examples/Aeson.hs
* http://hackage.haskell.org/package/wreq-0.5.3.2/docs/Network-Wreq.html#v:oauth2Bearer
* http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson-Types.html

-}

  

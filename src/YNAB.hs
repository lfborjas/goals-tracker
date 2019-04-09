{-# LANGUAGE OverloadedStrings #-}

module YNAB ( buildRequest
            , budgets
            ) where

{-
Module to deal with the YNAB API:

https://api.youneedabudget.com/

-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

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


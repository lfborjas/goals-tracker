{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

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

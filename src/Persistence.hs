{-# LANGUAGE OverloadedStrings #-}

module Persistence where

import Control.Applicative
import qualified System.Directory as Dir
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


data Account = Account
  { id_ :: Int
  , name :: T.Text
  , balance :: Double
  } deriving Show

instance FromRow Account where
  fromRow =
    Account
    <$> field -- id
    <*> field -- name
    <*> field -- balance

instance ToRow Account where
  toRow (Account id_ name balance) =
    toRow (id_, name, balance)

migrate :: IO ()
migrate = do
  -- TODO: ensure that the directory exists, maybe actually get it as a parameter?
  appDir <- (Dir.getAppUserDataDirectory "goalsTracker")
  let dbFilename = appDir ++ "/goals_tracker.db"
  conn <- open dbFilename
  execute_ conn $ mconcat ["CREATE TABLE IF NOT EXISTS accounts"
                          ,"(id INTEGER PRIMARY KEY,"
                          , "name TEXT,"
                          , "balance REAL"
                          ,")"]

  -- some test data
  execute conn "INSERT INTO accounts (name, balance) values (?,?)"
    ("test account" :: String, 333.33 :: Double)
  execute conn "INSERT INTO accounts (name, balance) values (?,?)"
    ("test account 2" :: String, 333.33 :: Double)

  r <- query_ conn "SELECT * from accounts" :: IO [Account]
  mapM_ print r
  close conn
  
  
{-
Using a modified version of the example in:

https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html

We get

λ> migrate
Account {id_ = 1, name = "test account", balance = 333.33}
Account {id_ = 2, name = "test account 2", balance = 333.33}


Next Steps:

in existing modules
* go back to using M.Account, figure out the actual model and then modify the JSON And Row
serializers to work with the bits they need.
* implement an accountHistory table that tracks balance over time

in Lib module
* plot a history of a given account
* implement a createFromAPI method in a new module, to persist an account from the API
* implement an updateAllFromAPI method in a new module, to update existing accounts from the API
* implement a Goal model, join table with Accounts?

in main:
* ???

open questions: to keep imports sane, should Models be importing Persistence and YNAB
and defining its serializers/helper methods there?


* implement the actual UI:
- make the hello world work
- plot some existing account data in gtk (make sure we can plot to begin with)
- 
-}

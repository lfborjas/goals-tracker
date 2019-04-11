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

-}

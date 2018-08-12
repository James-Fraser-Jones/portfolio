{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Database.HDBC.Sqlite3
import Database.HDBC
import qualified Data.Text as T
import System.Console.Haskeline
import System.Directory (doesFileExist)
import Data.Functor
import System.Random

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------------------------------
--Premade Sql Commands

type SqlCommand = String

createItemsTableSql :: SqlCommand
createItemsTableSql =
  "CREATE TABLE `items` (\
  \ `item` INTEGER,\
  \ `lot_number` INTEGER NOT NULL CHECK(lot_number > 0) UNIQUE,\
  \ `vendor` INTEGER NOT NULL CHECK(vendor > 0),\
  \ `description` TEXT NOT NULL,\
  \ `reserve` REAL CHECK(reserve > 0),\
  \ `pre_bid` REAL CHECK(pre_bid > 0),\
  \ `purchaser` INTEGER CHECK(purchaser > 0),\
  \ `sale_price` REAL CHECK(sale_price > 0),\
  \ PRIMARY KEY(`item`) )"

addItemSql :: SqlCommand
addItemSql =
  "INSERT INTO `items` (`lot_number`, `vendor`, `description`, `reserve`, `pre_bid`, `purchaser`, `sale_price`) VALUES (?, ?, ?, ?, ?, ?, ?)"

--------------------------------------------------------------------------------------------------------
--Database Actions

connect :: FilePath -> IO Connection
connect = connectSqlite3

--disconnect :: IConnection conn => conn -> IO ()

createTables :: IConnection conn => conn -> IO ()
createTables conn = do
  runRaw conn createItemsTableSql --create items table
  commit conn                     --commit changes

addItem :: IConnection conn => Item -> conn -> IO ()
addItem item conn = do
  addItem' <- prepare conn addItemSql --prepare statement for adding new items (SQL errors either occour here PREPARE)
  execute addItem' $ itemToSql item   --add the item                           (Or occour here EXECUTE)
  commit conn                         --commit changes

addItem2 a b c d e f g = addItem (Item a b c d e f g)

addItems :: IConnection conn => [Item] -> conn -> IO ()
addItems items conn = do
  addItems' <- prepare conn addItemSql        --prepare statement for adding new items
  executeMany addItems' $ itemToSql <$> items --add the items
  commit conn                                 --commit changes

addRand :: IConnection conn => Int -> conn -> IO ()
addRand n conn = do
  items <- itemsR n
  addItems items conn

-------------------------------------------------------------------------------------------------------
--Converting between Haskell and DB records

data Item = Item {
  lotNum :: Int,
  vendor :: Int,
  desc :: T.Text,
  reserve :: Maybe Double,
  preBid :: Maybe Double,
  purch :: Maybe Int,
  sPrice :: Maybe Double
} deriving Show

itemToSql :: Item -> [SqlValue]
itemToSql (Item a b c d e f g) = [toSql a, toSql b, toSql c, toSql d, toSql e, toSql f, toSql g]

sqlToItem :: [SqlValue] -> Item
sqlToItem [a,b,c,d,e,f,g] = Item (fromSql a) (fromSql b) (fromSql c) (fromSql d) (fromSql e) (fromSql f) (fromSql g)

ex1, ex2 :: Item
ex1 = Item 5 4 "Helicopter Ride" (Just 3.32) Nothing (Just 2) (Just 22.41)
ex2 = Item 33 23 "Brass Spoon" Nothing (Just 12.04) (Just 47) (Just 62.53)

-------------------------------------------------------------------------------------------------------
--Creating random records

itemsR :: Int -> IO [Item]
itemsR n = mapM itemR [1..n]

itemR :: Int -> IO Item
itemR lot = do
  book <- getBook
  Item lot <$> vendorR <*> descR book <*> doubleR (0,50) <*> doubleR (0,20) <*> purchR <*> doubleR (0,200)

getBook :: IO (IntMap T.Text)
getBook = do
  s <- readFile "words.txt"
  let f = IntMap.fromList . zip [0..9999] . map T.pack . words
  return $ f s

descR :: IntMap T.Text -> IO (T.Text)
descR book = do
  rand <- randomRIO (0,9999)
  rand2 <- randomRIO (0,9999)
  return $ book IntMap.! rand `T.append` " " `T.append` (book IntMap.! rand2)

vendorR :: IO Int
vendorR = randomRIO (1,1000)

purchR :: IO (Maybe Int)
purchR = do
  result <- randomIO
  if result
  then return Nothing
  else do
    rand <- randomRIO (1,1000)
    return $ Just rand

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

doubleR :: (Double, Double) -> IO (Maybe Double)
doubleR tup = do
  result <- randomIO
  if result
  then return Nothing
  else do
    rand <- randomRIO tup
    return $ Just $ truncate' rand 2

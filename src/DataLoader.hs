module DataLoader
    ( StockData(..)
    , loadStockData
    ) where

import Data.List.Split (splitOn)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)
import Control.Monad (when)
import System.Directory (doesFileExist)

-- Define a data structure to hold stock data
data StockData = StockData
    { date   :: Day
    , close  :: Double
    , ticker :: String
    } deriving (Show)

-- Parse a single line of CSV data into StockData
parseStockData :: String -> Maybe StockData
parseStockData line = do
    let fields = splitOn "," line
    when (length fields < 6) Nothing
    
    -- Parse date (field 1)
    dateStr <- Just (fields !! 0)
    date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr
    
    -- Parse close price (field 5, remove asterisks if present)
    closeStr <- Just (fields !! 4)
    let cleanClose = filter (/= '*') closeStr
    close <- readMaybe cleanClose
    
    -- Get ticker (field 6) and clean it
    let ticker = filter (\c -> c /= '\r' && c /= '\n') (fields !! 5)
    
    return $ StockData date close ticker

-- Load and parse stock data from CSV file
loadStockData :: FilePath -> IO [StockData]
loadStockData filePath = do
    fileExists <- doesFileExist filePath
    if not fileExists
        then do
            putStrLn $ "Error: File not found at " ++ filePath
            return []
        else do
            contents <- readFile filePath
            let ls = lines contents
                -- Skip the header line
                dataLines = drop 1 ls
                parsedData = mapMaybe parseStockData dataLines
            return parsedData

-- Helper function for handling Maybe values in a list context
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs
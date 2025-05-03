module Main where

import DataLoader (StockData(..), loadStockData)

main :: IO ()
main = do
    putStrLn "Loading stock data..."
    
    -- Load the stock data from CSV file
    let dataPath = "data/dow_jones_all_tickers_2024.csv"
    stockData <- loadStockData dataPath
    
    -- Print the number of records loaded
    putStrLn $ "Loaded " ++ show (length stockData) ++ " records."
    
    -- Print each record
    putStrLn "\nStock Data:"
    mapM_ printStockData stockData
    
    putStrLn "Done."

-- Simple function to print a stock data record
printStockData :: StockData -> IO ()
printStockData sd = putStrLn $ 
    "Date: " ++ show (date sd) ++ 
    ", Close: " ++ show (close sd) ++ 
    ", Ticker: " ++ ticker sd
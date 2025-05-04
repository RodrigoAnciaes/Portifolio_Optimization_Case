module Main where

import DataLoader (StockData(..), loadStockData)
import Simulate (Wallet, generateRandomWallets, printWallet)
import Data.List (nub, sortOn)

main :: IO ()
main = do
    putStrLn "Loading stock data..."
    
    -- Load the stock data from CSV file
    let dataPath = "data/stocks.csv"
    stockData <- loadStockData dataPath
    
    -- Print the number of records loaded
    putStrLn $ "Loaded " ++ show (length stockData) ++ " records."
    
    -- Extract and sort unique tickers
    let tickers = nub $ map ticker stockData
        sortedTickers = sortOn id tickers
    
    putStrLn $ "Found " ++ show (length tickers) ++ " unique tickers."
    putStrLn $ "Tickers: " ++ show sortedTickers
    
    -- Generate 1000 random wallets
    putStrLn "\nGenerating 1000 random wallets..."
    wallets <- generateRandomWallets 1000 stockData
    
    -- Print the first wallet as an example
    --putStrLn "\nExample of generated wallet:"
    --printWallet (head wallets) sortedTickers
    
    putStrLn $ "\nSuccessfully generated " ++ show (length wallets) ++ " random wallets."
    putStrLn "Done."
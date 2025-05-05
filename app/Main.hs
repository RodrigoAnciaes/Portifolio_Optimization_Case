module Main where

import DataLoader (StockData(..), loadStockData)
import Simulate (Wallet, generateRandomWallets, printWallet, printWalletWithReturn, calculateWalletReturns)
import Data.List (nub, sortOn)
import Data.Ord (comparing)
import Control.Monad (forM_)

main :: IO ()
main = do
    putStrLn "Loading stock data..."
   
    -- Load the stock data from CSV file
    let dataPath = "data/dow_jones_all_tickers_2024.csv"
    stockData <- loadStockData dataPath
   
    -- Print the number of records loaded
    putStrLn $ "Loaded " ++ show (length stockData) ++ " records."
   
    -- Extract and sort unique tickers
    let tickers = nub $ map ticker stockData
        sortedTickers = sortOn id tickers
   
    putStrLn $ "Found " ++ show (length tickers) ++ " unique tickers."
    putStrLn $ "Tickers: " ++ show sortedTickers
   
    -- Generate random wallets
    let numWallets = 1000  -- Reduced number for faster testing
    putStrLn $ "\nGenerating " ++ show numWallets ++ " random wallets..."
    wallets <- generateRandomWallets numWallets stockData

    putStrLn "\nExample wallet:"
    let exampleWallet = head wallets
    putStrLn $ "Wallet allocation: " ++ show exampleWallet
    
    -- Calculate annual returns for all wallets
    putStrLn "\nCalculating annual returns using parallel processing..."
    walletReturns <- calculateWalletReturns stockData wallets
    
    -- Sort wallets by return (highest first)
    let sortedWalletReturns = sortOn (negate . snd) walletReturns
    
    -- Print top 5 performing wallets
    putStrLn "\nTop 5 performing wallets:"
    forM_ (take 5 sortedWalletReturns) $ \walletReturn -> do
        printWalletWithReturn walletReturn sortedTickers
        putStrLn ""
    
    -- Print bottom 5 performing wallets
    -- putStrLn "\nBottom 5 performing wallets:"
    -- forM_ (take 5 $ reverse sortedWalletReturns) $ \walletReturn -> do
    --     printWalletWithReturn walletReturn sortedTickers
    --     putStrLn ""
    
    -- Print average return across all wallets
    -- putStrLn "\nCalculating average annual return across all wallets..."
    -- let avgReturn = sum (map snd walletReturns) / fromIntegral (length walletReturns)
    -- putStrLn $ "Average annual return across all wallets: " ++ show (avgReturn * 100) ++ "%"
    
    putStrLn $ "\nSuccessfully analyzed " ++ show (length wallets) ++ " random wallets."
    putStrLn "Done."
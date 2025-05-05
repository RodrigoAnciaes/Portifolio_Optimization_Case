module Main where

import DataLoader (StockData(..), loadStockData)
import Simulate (Wallet, generateRandomWallets, printWallet, 
                printWalletWithReturn, printWalletWithReturnAndVolatility, 
                calculateWalletReturnsAndVolatilities)
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
    
    -- Calculate annual returns and volatilities for all wallets
    putStrLn "\nCalculating annual returns and volatilities using parallel processing..."
    walletMetrics <- calculateWalletReturnsAndVolatilities stockData wallets
    
    -- Sort wallets by return (highest first)
    let sortedByReturn = sortOn (\(_, ret, _) -> negate ret) walletMetrics
    
    -- Print top 5 performing wallets by return
    putStrLn "\nTop 5 performing wallets by return:"
    forM_ (take 5 sortedByReturn) $ \walletMetric -> do
        printWalletWithReturnAndVolatility walletMetric sortedTickers
        putStrLn ""
    
    -- Sort wallets by Sharpe ratio (highest first)
    let riskFreeRate = 0.02 -- Assuming 2% risk-free rate
        sortedBySharpe = sortOn (\(_, ret, vol) -> negate $ (ret - riskFreeRate) / vol) walletMetrics
    
    -- Print top 5 wallets by Sharpe ratio
    putStrLn "\nTop 5 performing wallets by Sharpe ratio:"
    forM_ (take 5 sortedBySharpe) $ \walletMetric -> do
        printWalletWithReturnAndVolatility walletMetric sortedTickers
        putStrLn ""
    
    -- Print wallets with lowest volatility (for risk-averse investors)
    let sortedByVolatility = sortOn (\(_, _, vol) -> vol) walletMetrics
    
    putStrLn "\nTop 5 wallets with lowest volatility:"
    forM_ (take 5 sortedByVolatility) $ \walletMetric -> do
        printWalletWithReturnAndVolatility walletMetric sortedTickers
        putStrLn ""
    
    -- Print average metrics across all wallets
    putStrLn "\nCalculating average metrics across all wallets..."
    let avgReturn = sum (map (\(_, ret, _) -> ret) walletMetrics) / fromIntegral (length walletMetrics)
        avgVolatility = sum (map (\(_, _, vol) -> vol) walletMetrics) / fromIntegral (length walletMetrics)
        avgSharpe = sum (map (\(_, ret, vol) -> (ret - riskFreeRate) / vol) walletMetrics) / fromIntegral (length walletMetrics)
    
    putStrLn $ "Average annual return: " ++ show (avgReturn * 100) ++ "%"
    putStrLn $ "Average annual volatility: " ++ show (avgVolatility * 100) ++ "%"
    putStrLn $ "Average Sharpe ratio: " ++ show avgSharpe
    
    putStrLn $ "\nSuccessfully analyzed " ++ show (length wallets) ++ " random wallets."
    putStrLn "Done."
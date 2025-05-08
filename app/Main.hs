module Main where

import DataLoader (StockData(..), loadStockData)
import Simulate (Wallet, generateRandomWallets, printWallet, 
                printWalletWithReturn, printWalletWithReturnAndVolatility, 
                calculateWalletReturnsAndVolatilities, calculateSharpeRatio)
import Data.List (nub, sortOn, maximumBy)
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
    let numWallets = 5000  -- Reduced number for faster testing
    putStrLn $ "\nGenerating " ++ show numWallets ++ " random wallets..."
    wallets <- generateRandomWallets numWallets stockData

    putStrLn "\nExample wallet:"
    let exampleWallet = head wallets
    printWallet exampleWallet sortedTickers Nothing
    
    -- Set risk-free rate
    let riskFreeRate = 0.14 -- Assuming 14% risk-free rate
    
    -- Calculate annual returns and volatilities for all wallets
    putStrLn "\nCalculating annual returns and volatilities using parallel processing..."
    walletMetrics <- calculateWalletReturnsAndVolatilities stockData wallets
    
    -- Calculate Sharpe ratios for all wallets
    let walletSharpes = map (\(wallet, ret, vol) -> 
                               (wallet, ret, vol, calculateSharpeRatio riskFreeRate ret vol)) 
                            walletMetrics
    
    -- Find the wallet with the best Sharpe ratio
    let bestSharpeWallet = maximumBy (comparing (\(_, _, _, sharpe) -> sharpe)) walletSharpes
        (bestWallet, bestReturn, bestVol, bestSharpe) = bestSharpeWallet
    
    
    -- Print average metrics across all wallets
    putStrLn "\nCalculating average metrics across all wallets..."
    let avgReturn = sum (map (\(_, ret, _, _) -> ret) walletSharpes) / fromIntegral (length walletSharpes)
        avgVolatility = sum (map (\(_, _, vol, _) -> vol) walletSharpes) / fromIntegral (length walletSharpes)
        avgSharpe = sum (map (\(_, _, _, sharpe) -> sharpe) walletSharpes) / fromIntegral (length walletSharpes)
    
    putStrLn $ "Average annual return: " ++ show (avgReturn * 100) ++ "%"
    putStrLn $ "Average annual volatility: " ++ show (avgVolatility * 100) ++ "%"
    putStrLn $ "Average Sharpe ratio: " ++ show avgSharpe
    
    -- Print the wallet with the best Sharpe ratio
    putStrLn "\n=== OPTIMAL PORTFOLIO BY SHARPE RATIO ==="
    putStrLn $ "Best Sharpe ratio: " ++ show bestSharpe
    printWalletWithReturnAndVolatility (bestWallet, bestReturn, bestVol) sortedTickers riskFreeRate
    
    putStrLn $ "\nSuccessfully analyzed " ++ show (length wallets) ++ " random wallets."
    putStrLn "Done."
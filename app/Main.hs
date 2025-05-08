module Main where

import DataLoader (StockData(..), loadStockData)
import Simulate (Wallet, generateWalletsByTickerCombination, printWallet, 
                printWalletWithReturn, printWalletWithReturnAndVolatility, 
                calculateWalletReturnsAndVolatilities, calculateSharpeRatio)
import Data.List (nub, sortOn, maximumBy)
import Data.Ord (comparing)
import Control.Monad (forM_)
import Control.Parallel.Strategies (parBuffer, rdeepseq, withStrategy)
import qualified Data.Vector as V

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
   
    -- Set the number of random wallets per ticker combination
    let walletsPerCombination = 50
    
    -- Calculate the total number of ticker combinations (n choose 25)
    let numTickers = length sortedTickers
        nCombinations = binomial (toInteger numTickers) 25
    
    putStrLn $ "\nGenerating " ++ show walletsPerCombination ++ " random wallets for each of " 
             ++ show nCombinations ++ " ticker combinations..."
    
    -- Generate wallets for each ticker combination
    allCombinationWallets <- generateWalletsByTickerCombination walletsPerCombination stockData
    
    -- Display an example of one combination
    let exampleCombo = head allCombinationWallets
        ((exampleIndices, exampleTickers), exampleWallets) = exampleCombo
    
    putStrLn "\nExample ticker combination:"
    putStrLn $ "Selected indices: " ++ show exampleIndices
    putStrLn $ "Selected tickers: " ++ show exampleTickers
    
    putStrLn "\nExample wallet from this combination:"
    let exampleWallet = head exampleWallets
    printWallet exampleWallet sortedTickers Nothing
    
    -- Set risk-free rate
    let riskFreeRate = 0.14 -- Assuming 14% risk-free rate
    
    -- Process each combination and find the best wallet
    putStrLn "\nCalculating metrics for all wallet combinations..."
    
    -- Create a flat list of all wallets
    let allWallets = concatMap snd allCombinationWallets
    
    putStrLn $ "Total number of wallets to evaluate: " ++ show (length allWallets)
    
    -- Calculate annual returns and volatilities for all wallets
    putStrLn "Calculating annual returns and volatilities using parallel processing..."
    walletMetrics <- calculateWalletReturnsAndVolatilities stockData allWallets
    
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
    
    -- Find which combination the best wallet belongs to
    let bestWalletIndices = [i | i <- [0..V.length bestWallet - 1], bestWallet V.! i > 0]
    
    putStrLn "\nThe optimal portfolio uses the following tickers:"
    putStrLn $ show $ map (\idx -> sortedTickers !! idx) bestWalletIndices
    
    putStrLn $ "\nSuccessfully analyzed " ++ show (length allWallets) ++ " wallets across " 
             ++ show (length allCombinationWallets) ++ " ticker combinations."
    putStrLn "Done."

-- Helper function to calculate binomial coefficient (n choose k)
binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | k > n - k = binomial n (n - k)
    | otherwise = binomial (n - 1) (k - 1) * n `div` k
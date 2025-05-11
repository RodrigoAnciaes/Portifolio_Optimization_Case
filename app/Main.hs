module Main where

import DataLoader (StockData(..), loadStockData)
import Portfolio (Wallet, generateAllTickerCombinations, printWalletWithReturnAndVolatility)
import Returns (calculateDailyReturns, calculateCovarianceMatrix)
import Optimization (processAllCombinations)

import Data.List (nub, sortOn)
import qualified Data.Vector as V
import System.Random (getStdGen)

main :: IO ()
main = do
    -- Load stock data
    putStrLn "Loading stock data..."
    let dataPath = "data/dow_jones_all_tickers_2024.csv"
    stockData <- loadStockData dataPath
    putStrLn $ "Loaded " ++ show (length stockData) ++ " records."
   
    -- Extract tickers
    let tickers = nub $ map ticker stockData
        sortedTickers = sortOn id tickers
        numTickers = length sortedTickers
    putStrLn $ "Found " ++ show numTickers ++ " unique tickers."
    
    -- Pre-compute daily returns
    putStrLn "Pre-computing daily returns..."
    let dailyReturns = calculateDailyReturns stockData
    
    -- Generate ticker combinations
    putStrLn "Generating all ticker combinations (25 out of 30)..."
    let allCombinations = generateAllTickerCombinations sortedTickers
        totalCombinations = length allCombinations
    putStrLn $ "Total number of combinations: " ++ show totalCombinations
    
    -- Configuration
    let walletsPerCombination = 1000
        riskFreeRate = 0.14
    
    -- Initialize random generator
    gen <- getStdGen
    
    -- Calculate covariance matrix
    putStrLn "Calculating covariance matrix (this might take a while)..."
    covMatrix <- calculateCovarianceMatrix stockData sortedTickers
    putStrLn "Covariance matrix calculated."
    
    -- Initialize with worst values
    let initialBest = (V.replicate numTickers 0, 0.0, 0.0, -999.0)  -- (wallet, return, volatility, sharpe)
    
    -- Start optimization
    putStrLn $ "\nStarting exhaustive evaluation of all combinations..."
    putStrLn $ "Processing " ++ show walletsPerCombination ++ " random weight allocations per combination."
    putStrLn "Progress: 0.00% - Best Sharpe: N/A"
    
    -- Process each combination
    finalBest <- processAllCombinations 
                    initialBest 
                    gen 
                    1 
                    allCombinations 
                    totalCombinations
                    numTickers
                    walletsPerCombination
                    dailyReturns
                    sortedTickers
                    covMatrix
                    riskFreeRate
    
    putStrLn "\n\nExhaustive evaluation complete!"
    
    -- Unpack the final best wallet
    let (bestWallet, bestReturn, bestVol, bestSharpe) = finalBest
    
    -- Print the best portfolio
    putStrLn "\n=== OPTIMAL PORTFOLIO BY SHARPE RATIO ==="
    putStrLn $ "Best Sharpe ratio: " ++ show bestSharpe
    printWalletWithReturnAndVolatility (bestWallet, bestReturn, bestVol) sortedTickers riskFreeRate
    
    -- Find which tickers are used in the optimal portfolio
    let bestWalletIndices = [i | i <- [0..V.length bestWallet - 1], bestWallet V.! i > 0]
    
    putStrLn "\nThe optimal portfolio uses the following tickers:"
    putStrLn $ show $ map (\idx -> sortedTickers !! idx) bestWalletIndices
    
    putStrLn "\nAnalysis complete."
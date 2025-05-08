module Main where

import DataLoader (StockData(..), loadStockData)
import Simulate (Wallet, generateAllTickerCombinations, printWallet, 
                printWalletWithReturn, printWalletWithReturnAndVolatility,
                calculateWalletVolatility, calculateSharpeRatio, calculateCovarianceMatrix)
import Data.List (nub, sortOn, maximumBy)
import Data.Ord (comparing)
import Control.Monad (foldM, when)
import System.Random
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Time (Day)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- Calculate daily returns from stock data (moved from Simulate.hs)
calculateDailyReturns :: [StockData] -> M.Map (Day, String) Double
calculateDailyReturns stockData =
    let groupedByTicker = sortOn ticker stockData
        -- Calculate daily returns for each ticker series
        tickerReturns = calcTickerReturns groupedByTicker
    in M.fromList $ concat tickerReturns
    where
        calcTickerReturns [] = []
        calcTickerReturns stocks = 
            let currentTicker = ticker (head stocks)
                tickerGroup = takeWhile (\s -> ticker s == currentTicker) stocks
                restStocks = dropWhile (\s -> ticker s == currentTicker) stocks
                
                -- Calculate returns for this ticker
                sortedByDate = sortOn date tickerGroup
                pairwise = zip sortedByDate (tail sortedByDate)
                returns = map (\(prev, curr) -> 
                        ((date curr, ticker curr), (close curr - close prev) / close prev)) pairwise
            in returns : calcTickerReturns restStocks

-- Calculate annual return for a wallet
calculateWalletReturn :: M.Map (Day, String) Double -> [String] -> Wallet -> Double
calculateWalletReturn dailyReturns tickers wallet =
    let -- Get all unique dates
        dates = nub $ map fst $ map fst $ M.toList dailyReturns
        
        -- Calculate daily returns for this wallet
        walletDailyReturns = map (calculateWalletReturnForDate dailyReturns tickers wallet) dates
        
        -- Calculate annual return (mean daily return * 252)
        validReturns = filter (not . isNaN) walletDailyReturns
        meanDailyReturn = if null validReturns 
                          then 0
                          else sum validReturns / fromIntegral (length validReturns)
        annualReturn = meanDailyReturn * 252
    in annualReturn
    where
        calculateWalletReturnForDate :: M.Map (Day, String) Double -> [String] -> Wallet -> Day -> Double
        calculateWalletReturnForDate returnMap tickers wallet day =
            -- Multiply each stock's weight by its return for the day, and sum
            V.sum $ V.imap (\i weight -> 
                let ticker = tickers !! i
                    stockReturn = M.lookup (day, ticker) returnMap
                in case stockReturn of
                    Just ret -> weight * ret
                    Nothing -> 0) wallet

-- Generate a single random wallet with weights for selected indices
generateRandomWallet :: StdGen -> [Int] -> Int -> (Wallet, StdGen)
generateRandomWallet gen selectedIndices walletSize =
    let
        -- Generate initial random weights
        (rawWeights, gen') = genRandoms 25 [] gen
        
        -- Apply the 20% cap and redistribute excess
        cappedWeights = enforceMaxWeight 0.2 rawWeights
        
        -- Normalize weights to sum to 1.0
        sumWeights = sum cappedWeights
        normalizedWeights = map (/ sumWeights) cappedWeights
        
        -- Create a vector of walletSize positions with zeros
        emptyWallet = V.replicate walletSize 0.0
        
        -- Fill in the selected 25 positions with normalized weights
        wallet = foldr (\(idx, weight) acc -> V.update acc (V.singleton (idx, weight)))
                       emptyWallet
                       (zip selectedIndices normalizedWeights)
    in
        (wallet, gen')
    where
        -- Helper to generate n random values between 0 and 1
        genRandoms :: Int -> [Double] -> StdGen -> ([Double], StdGen)
        genRandoms 0 acc gen = (acc, gen)
        genRandoms n acc gen =
            let (r, gen') = randomR (0.1, 1.0) gen
            in genRandoms (n-1) (r:acc) gen'
        
        -- Helper to enforce the maximum weight constraint and redistribute excess
        enforceMaxWeight :: Double -> [Double] -> [Double]
        enforceMaxWeight maxWeight weights = 
            -- Continue until no weight exceeds the maximum
            if all (<= maxWeight * sum weights) weights
                then weights
                else enforceMaxWeight maxWeight (redistributeExcess maxWeight weights)
        
        -- Redistribute excess weight from items exceeding the maximum
        redistributeExcess :: Double -> [Double] -> [Double]
        redistributeExcess maxWeight weights =
            let totalSum = sum weights
                maxAllowed = maxWeight * totalSum
                (exceeders, compliant) = span (> maxAllowed) (sortOn negate weights)
                totalExcess = sum exceeders - (maxAllowed * fromIntegral (length exceeders))
                excessPerItem = totalExcess / fromIntegral (length compliant)
                adjustedCompliant = map (+ excessPerItem) compliant
            in replicate (length exceeders) maxAllowed ++ adjustedCompliant

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
        numTickers = length sortedTickers
   
    putStrLn $ "Found " ++ show numTickers ++ " unique tickers."
    
    -- Calculate daily returns once for all tickers (reused for all wallets)
    putStrLn "Pre-computing daily returns..."
    let dailyReturns = calculateDailyReturns stockData
    
    -- Generate all ticker combinations
    putStrLn "Generating all ticker combinations (25 out of 30)..."
    let allCombinations = generateAllTickerCombinations sortedTickers
        totalCombinations = length allCombinations
    
    putStrLn $ "Total number of combinations: " ++ show totalCombinations
    
    -- Set number of wallets per combination
    let walletsPerCombination = 10
    
    -- Calculate covariance matrix once (memory intensive but necessary)
    putStrLn "Calculating covariance matrix (this might take a while)..."
    covMatrix <- calculateCovarianceMatrix stockData sortedTickers
    putStrLn "Covariance matrix calculated."
    
    -- Initialize random generator and risk-free rate
    gen <- getStdGen
    let riskFreeRate = 0.14
    
    -- Initialize with worst values
    let initialBest = (V.replicate numTickers 0, 0.0, 0.0, -999.0)  -- (wallet, return, volatility, sharpe)
    
    -- Process combinations sequentially
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

-- Helper function to process all combinations recursively
processAllCombinations :: (Wallet, Double, Double, Double) -> StdGen -> Int -> [([Int], [String])] -> Int -> Int -> Int
                        -> M.Map (Day, String) Double -> [String] -> V.Vector (V.Vector Double) -> Double 
                        -> IO (Wallet, Double, Double, Double)
processAllCombinations bestSoFar _ _ [] _ _ _ _ _ _ _ = 
    return bestSoFar  -- Base case: no more combinations to process
    
processAllCombinations bestSoFar@(_, _, _, bestSharpe) gen count (combo:restCombos) total numTickers walletsPerCombo dailyReturns tickers covMatrix riskFree = do
    -- Update progress periodically
    when (count `mod` 10 == 0 || count == 1 || count == total) $ do
        let progress = (fromIntegral count / fromIntegral total) * 100 :: Double
        printf "\rProgress: %.2f%% - Best Sharpe: %.4f" progress bestSharpe
        hFlush stdout
    
    -- Process this combination
    let (indices, _) = combo
    
    -- Process multiple wallets for this combination
    newBest <- foldM 
        (\currentBest@(_, _, _, currentSharpe) _ -> do
            -- Generate a random wallet
            let (wallet, newGen) = generateRandomWallet gen indices numTickers
                
                -- Calculate return
                ret = calculateWalletReturn dailyReturns tickers wallet
                
                -- Calculate volatility
                vol = calculateWalletVolatility wallet covMatrix
                
                -- Calculate Sharpe ratio
                sharpe = calculateSharpeRatio riskFree ret vol
            
            -- Return the better wallet
            return $ if sharpe > currentSharpe
                     then (wallet, ret, vol, sharpe)
                     else currentBest
        )
        bestSoFar
        [1..walletsPerCombo]
    
    -- Continue with the next combination
    processAllCombinations newBest gen (count + 1) restCombos total numTickers walletsPerCombo dailyReturns tickers covMatrix riskFree

-- Helper function to calculate binomial coefficient (n choose k)
binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | k > n - k = binomial n (n - k)
    | otherwise = binomial (n - 1) (k - 1) * n `div` k
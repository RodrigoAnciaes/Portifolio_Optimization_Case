module Portfolio
    ( Wallet
    , TickerCombination
    , generateRandomWallet
    , calculateWalletVolatility
    , calculateSharpeRatio
    , printWallet
    , printWalletWithReturn
    , printWalletWithReturnAndVolatility
    , generateAllTickerCombinations
    ) where

import System.Random
import Data.List (nub, sortOn)
import qualified Data.Vector as V
import Data.Time (Day)

-- Type definition for a wallet - a vector of weights
type Wallet = V.Vector Double

-- Type for a ticker combination - (indices, tickers)
type TickerCombination = ([Int], [String])

-- Generate a single random wallet with weights for selected indices
-- Using the original function signature for consistency
generateRandomWallet :: RandomGen g => g -> ([Int], [String]) -> (Wallet, g)
generateRandomWallet gen (selectedIndices, _) =
    let
        -- Generate initial random weights
        (rawWeights, gen') = genRandoms 25 [] gen
        
        -- Apply the 20% cap and redistribute excess
        cappedWeights = enforceMaxWeight 0.2 rawWeights
        
        -- Normalize weights to sum to 1.0
        sumWeights = sum cappedWeights
        normalizedWeights = map (/ sumWeights) cappedWeights
        
        -- Create a vector of 30 positions with zeros
        emptyWallet = V.replicate 30 0.0
        
        -- Fill in the selected positions with normalized weights
        wallet = foldr (\(idx, weight) acc -> V.update acc (V.singleton (idx, weight)))
                       emptyWallet
                       (zip selectedIndices normalizedWeights)
    in
        (wallet, gen')

-- Helper to generate n random values between 0 and 1
genRandoms :: RandomGen g => Int -> [Double] -> g -> ([Double], g)
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

-- Calculate volatility for a wallet
calculateWalletVolatility :: Wallet -> V.Vector (V.Vector Double) -> Double
calculateWalletVolatility wallet covMatrix =
    let n = V.length wallet
        -- Compute w_t * e * w
        innerProducts = V.generate n $ \i ->
            let weight_i = wallet V.! i
                covRow = covMatrix V.! i
            in weight_i * V.sum (V.imap (\j weight_j -> weight_j * (covRow V.! j)) wallet)
        variance = V.sum innerProducts
        -- Volatility = sqrt(252) * sqrt(variance)
        annualizedVolatility = sqrt(252) * sqrt(variance)
    in annualizedVolatility

-- Calculate Sharpe ratio for a given return and volatility
calculateSharpeRatio :: Double -> Double -> Double -> Double
calculateSharpeRatio riskFreeRate ret vol = 
    if vol > 0 then (ret - riskFreeRate) / vol else 0

-- Print the complete wallet allocation
printWallet :: Wallet -> [String] -> Maybe Double -> IO ()
printWallet wallet tickers mbReturn = do
    putStrLn "Wallet allocation (complete):"
    
    -- Get all non-zero holdings sorted by weight (highest to lowest)
    let nonZeroWeights = [(idx, w) | idx <- [0..V.length wallet - 1], let w = wallet V.! idx, w > 0]
        sortedWeights = sortOn (negate . snd) nonZeroWeights
    
    -- Print all holdings
    mapM_ (\(idx, weight) ->
        -- Make sure idx is within range of tickers array
        if idx < length tickers 
        then putStrLn $ "  " ++ tickers !! idx ++ ": " ++ showPercentage weight
        else putStrLn $ "  Unknown ticker (index " ++ show idx ++ "): " ++ showPercentage weight
        ) sortedWeights
    
    -- Print total and count of stocks
    let nonZeroCount = length $ filter (> 0) $ V.toList wallet
    putStrLn $ "Total allocation: " ++ showPercentage (V.sum wallet)
    putStrLn $ "Number of stocks: " ++ show nonZeroCount
    
    -- Print annual return if available
    case mbReturn of
        Just ret -> putStrLn $ "Annual return: " ++ showPercentage ret
        Nothing -> return ()
    
    where
        showPercentage :: Double -> String
        showPercentage value = show (value * 100) ++ "%"

-- Print a wallet with its return
printWalletWithReturn :: (Wallet, Double) -> [String] -> IO ()
printWalletWithReturn (wallet, ret) tickers = printWallet wallet tickers (Just ret)

-- Print a wallet with its return and volatility
printWalletWithReturnAndVolatility :: (Wallet, Double, Double) -> [String] -> Double -> IO ()
printWalletWithReturnAndVolatility (wallet, ret, vol) tickers riskFreeRate = do
    -- Print basic wallet info and return
    printWallet wallet tickers (Just ret)
    
    -- Print volatility
    putStrLn $ "Annual volatility: " ++ showPercentage vol
    
    -- Print Sharpe ratio
    let sharpeRatio = calculateSharpeRatio riskFreeRate ret vol
    putStrLn $ "Sharpe ratio: " ++ show sharpeRatio
    
    where
        showPercentage :: Double -> String
        showPercentage value = show (value * 100) ++ "%"

-- Helper function to generate combinations of k elements from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs)
    | k > length (x:xs) = []
    | otherwise = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- Generate all combinations of 25 tickers from the available ones
generateAllTickerCombinations :: [String] -> [TickerCombination]
generateAllTickerCombinations tickers = 
    let n = length tickers
        -- Generate all combinations of 25 indices from 0 to n-1
        allCombinations = combinations 25 [0..n-1]
        -- For each combination, get the corresponding tickers
        combinationsWithTickers = map (\indices -> 
            let selectedTickers = map (tickers !!) indices
            in (indices, selectedTickers)) allCombinations
    in combinationsWithTickers
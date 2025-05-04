module Simulate
    ( Wallet
    , generateRandomWallets
    , printWallet
    ) where

import System.Random
import Data.List (nub, sortOn)
import Control.Monad (replicateM)
import qualified Data.Vector as V
import DataLoader (StockData(..))

-- Type definition for a wallet - a vector of 30 weights
type Wallet = V.Vector Double

-- Generate a single random wallet with exactly 25 non-zero weights
-- The sum of all weights will be 1.0
-- No stock can have more than 20% allocation (0.2)
generateRandomWallet :: RandomGen g => g -> ([String], [Int]) -> (Wallet, g)
generateRandomWallet gen (tickers, selectedIndices) =
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
        
        -- Fill in the selected 25 positions with normalized weights
        wallet = foldr (\(idx, weight) acc -> V.update acc (V.singleton (idx, weight)))
                       emptyWallet
                       (zip selectedIndices normalizedWeights)
    in
        (wallet, gen')
    where
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

-- Select 25 random tickers from the list of all tickers
selectRandomTickers :: RandomGen g => g -> [String] -> ([Int], g)
selectRandomTickers gen allTickers =
    let tickerCount = length allTickers
        -- Generate indices to select 25 out of 30 tickers
        (selectedIndices, gen') = selectKFromN 25 tickerCount gen []
    in
        (selectedIndices, gen')
    where
        -- Helper to select k random indices from 0 to n-1 without duplicates
        selectKFromN :: RandomGen g => Int -> Int -> g -> [Int] -> ([Int], g)
        selectKFromN 0 _ gen acc = (acc, gen)
        selectKFromN k n gen acc =
            let (r, gen') = randomR (0, n-1) gen
            in if r `elem` acc
               then selectKFromN k n gen' acc
               else selectKFromN (k-1) n gen' (r:acc)

-- Generate n random wallets
generateRandomWallets :: Int -> [StockData] -> IO [Wallet]
generateRandomWallets n stockData = do
    gen <- getStdGen
   
    -- Extract unique tickers from stock data and sort them
    let allTickers = nub $ map ticker stockData
        sortedTickers = sortOn id allTickers
   
    -- Generate a list of indices to use for each wallet
    let (selectedIndices, gen') = selectRandomTickers gen sortedTickers
   
    -- Generate n random wallets
    return $ fst $ foldr (\_ (wallets, g) ->
                         let (wallet, g') = generateRandomWallet g (sortedTickers, selectedIndices)
                         in (wallet:wallets, g'))
                         ([], gen')
                         [1..n]

-- Pretty print a wallet with ticker names
printWallet :: Wallet -> [String] -> IO ()
printWallet wallet tickers = do
    putStrLn "Wallet allocation:"
    V.imapM_ (\idx weight ->
        when (weight > 0) $
            putStrLn $ "  " ++ tickers !! idx ++ ": " ++ showPercentage weight) wallet
    putStrLn $ "Total: " ++ showPercentage (V.sum wallet)
    where
        showPercentage :: Double -> String
        showPercentage value = show (value * 100) ++ "%"

-- Helper for when keyword
when :: Bool -> IO () -> IO ()
when cond action = if cond then action else return ()
module Simulate
    ( Wallet
    , generateRandomWallets
    , generateAllTickerCombinations
    , generateWalletsByTickerCombination
    , printWallet
    , printWalletWithReturn
    , calculateWalletReturns
    , calculateWalletVolatility
    , calculateSharpeRatio
    , printWalletWithReturnAndVolatility
    , calculateWalletReturnsAndVolatilities
    , calculateCovarianceMatrix
    ) where

import System.Random
import Data.List (nub, sortOn, groupBy, elemIndex, unfoldr)
import Data.Function (on)
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Control.DeepSeq (NFData, rnf, force)
import Control.Parallel (par, pseq)
import Control.Concurrent (getNumCapabilities)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Time (Day)
import DataLoader (StockData(..))

-- Re-export the same type and functions that were defined in this module originally
-- These were moved to separate modules but we keep them here for backward compatibility
import Portfolio
import Returns
import Utilities

-- These functions aren't moved yet - we keep them here 
-- to maintain compatibility with existing code

-- Generate a list of random wallets using the original method
-- Deprecated: Use Portfolio and Returns modules directly
generateRandomWallets :: Int -> [StockData] -> IO [Wallet]
generateRandomWallets n stockData = do
    gen <- getStdGen
   
    -- Extract unique tickers from stock data and sort them
    let allTickers = nub $ map ticker stockData
        sortedTickers = sortOn id allTickers
   
    -- Generate a list of indices to use for each wallet
    let (selectedIndices, gen') = selectRandomTickers gen sortedTickers
   
    -- Generate seeds for parallel wallet generation
    let seeds = take n $ unfoldr (Just . split) gen'
        
    -- Generate wallets in parallel using the seeds
    let wallets = withStrategy (parBuffer 100 rdeepseq) $ 
                  map (\g -> fst $ generateRandomWalletOrig g (sortedTickers, selectedIndices)) seeds
    
    return wallets

-- Helper: Select 25 random tickers from the list of all tickers
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

-- NEW: Generate wallets for each ticker combination in parallel
generateWalletsByTickerCombination :: Int -> [StockData] -> IO [(TickerCombination, [Wallet])]
generateWalletsByTickerCombination walletsPerCombination stockData = do
    gen <- getStdGen
    
    -- Extract unique tickers from stock data and sort them
    let allTickers = nub $ map ticker stockData
        sortedTickers = sortOn id allTickers
    
    -- Generate all combinations of 25 tickers
    let tickerCombinations = generateAllTickerCombinations sortedTickers
    
    -- Generate seeds for parallel wallet generation
    let seeds = take (length tickerCombinations) $ unfoldr (Just . split) gen
    
    -- Generate wallets for each combination in parallel
    let combinationWallets = withStrategy (parBuffer 100 rdeepseq) $ 
                            zipWith (\seed combo -> 
                                (combo, generateWalletsForCombination walletsPerCombination seed combo)) 
                            seeds tickerCombinations
    
    return combinationWallets

-- Helper: Generate wallets for a specific ticker combination
generateWalletsForCombination :: RandomGen g => Int -> g -> TickerCombination -> [Wallet]
generateWalletsForCombination count gen (indices, selectedTickers) =
    let allTickers = selectedTickers  -- Use the selected tickers directly
        (wallets, _) = generateWallets count gen [] (allTickers, indices)
    in wallets
    where
        generateWallets :: RandomGen g => Int -> g -> [Wallet] -> ([String], [Int]) -> ([Wallet], g)
        generateWallets 0 g acc _ = (acc, g)
        generateWallets n g acc tickerInfo =
            let (wallet, g') = generateRandomWalletOrig g tickerInfo
            in generateWallets (n-1) g' (wallet:acc) tickerInfo

-- Calculate returns and volatilities for all wallets
calculateWalletReturnsAndVolatilities :: [StockData] -> [Wallet] -> IO [(Wallet, Double, Double)]
calculateWalletReturnsAndVolatilities stockData wallets = do
    -- Extract unique tickers in order
    let allTickers = nub $ map ticker stockData
        sortedTickers = sortOn id allTickers
    
    -- Calculate returns for all wallets
    walletReturns <- calculateWalletReturns stockData wallets
    
    putStrLn "Calculating covariance matrix using parallel processing..."
    -- Calculate covariance matrix
    covMatrix <- calculateCovarianceMatrix stockData sortedTickers
    
    putStrLn "Calculating volatilities for all wallets..."
    -- Calculate volatilities for all wallets in parallel
    let walletVolatilities = withStrategy (parBuffer 100 rdeepseq) $ map (\(wallet, ret) -> 
            let vol = calculateWalletVolatility wallet covMatrix
            in (wallet, ret, vol)) walletReturns
    
    return walletVolatilities

-- Calculate annual returns for all wallets
calculateWalletReturns :: [StockData] -> [Wallet] -> IO [(Wallet, Double)]
calculateWalletReturns stockData wallets = do
    -- Extract unique tickers in order
    let allTickers = nub $ map ticker stockData
        sortedTickers = sortOn id allTickers
    
    -- Calculate daily returns for all stocks
    let dailyReturns = calculateDailyReturns stockData
    
    -- Calculate daily returns for each wallet
    let walletDailyReturns = calculateWalletDailyReturns dailyReturns sortedTickers wallets
    
    -- Calculate annual return for each wallet (mean daily return * 252)
    let annualReturns = map (\wallet -> 
            let dailyReturnsByDate = M.elems walletDailyReturns
                walletDailyReturnsList = concat 
                    [filter (not . isNaN) $ map (!! idx) dailyReturnsByDate | 
                     idx <- [0..length wallets - 1], 
                     idx == walletIdx wallet wallets]
                meanDailyReturn = if null walletDailyReturnsList 
                                  then 0
                                  else sum walletDailyReturnsList / fromIntegral (length walletDailyReturnsList)
                annualReturn = meanDailyReturn * 252
            in (wallet, annualReturn)) wallets
    
    -- Using parallelization to compute annual returns
    return $ parMap rdeepseq id annualReturns
    where
        -- Helper to find index of a wallet in the list
        walletIdx :: Wallet -> [Wallet] -> Int
        walletIdx wallet wallets = 
            case elemIndex wallet wallets of
                Just idx -> idx
                Nothing -> -1

-- Helper: Calculate daily returns for wallets
calculateWalletDailyReturns :: M.Map (Day, String) Double -> [String] -> [Wallet] -> M.Map Day [Double]
calculateWalletDailyReturns dailyReturns tickers wallets =
    let -- Get all unique dates
        dates = nub $ map fst $ map fst $ M.toList dailyReturns
        
        -- For each date, calculate return for each wallet, in parallel
        dateReturnsMap = M.fromList $ map (\d -> 
            (d, parMap rdeepseq (calculateWalletReturnForDate d dailyReturns tickers) wallets)) dates
    in dateReturnsMap
    where
        calculateWalletReturnForDate :: Day -> M.Map (Day, String) Double -> [String] -> Wallet -> Double
        calculateWalletReturnForDate day returnMap tickers wallet =
            -- Multiply each stock's weight by its return for the day, and sum
            V.sum $ V.imap (\i weight -> 
                let ticker = tickers !! i
                    stockReturn = M.lookup (day, ticker) returnMap
                in case stockReturn of
                    Just ret -> weight * ret
                    Nothing -> 0) wallet

-- Helper for when keyword
when :: Bool -> IO () -> IO ()
when cond action = if cond then action else return ()
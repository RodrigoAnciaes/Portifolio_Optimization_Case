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

-- Type definition for a wallet - a vector of 30 weights
type Wallet = V.Vector Double

-- Type for a ticker combination - (indices, tickers)
type TickerCombination = ([Int], [String])

-- Generate a single random wallet with specified weights for selected tickers
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

-- Helper function to generate combinations of k elements from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs)
    | k > length (x:xs) = []
    | otherwise = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- NEW: Generate all combinations of 25 tickers from the 30 available
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

-- NEW: Generate x random wallets for a specific ticker combination
generateWalletsForCombination :: RandomGen g => Int -> g -> TickerCombination -> [Wallet]
generateWalletsForCombination count gen (indices, selectedTickers) =
    let allTickers = selectedTickers  -- Use the selected tickers directly
        (wallets, _) = generateWallets count gen [] (allTickers, indices)
    in wallets
    where
        generateWallets :: RandomGen g => Int -> g -> [Wallet] -> ([String], [Int]) -> ([Wallet], g)
        generateWallets 0 g acc _ = (acc, g)
        generateWallets n g acc tickerInfo =
            let (wallet, g') = generateRandomWallet g tickerInfo
            in generateWallets (n-1) g' (wallet:acc) tickerInfo

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

-- Helper for original function: Select 25 random tickers from the list of all tickers
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

-- Generate n random wallets using parallelization (original function kept for compatibility)
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
                  map (\g -> fst $ generateRandomWallet g (sortedTickers, selectedIndices)) seeds
    
    return wallets

-- Calculate daily returns from stock data
calculateDailyReturns :: [StockData] -> M.Map (Day, String) Double
calculateDailyReturns stockData =
    -- Group by ticker
    let groupedByTicker = groupBy ((==) `on` ticker) $ sortOn ticker stockData
        -- Calculate daily returns for each ticker series
        tickerReturns = map calcTickerReturns groupedByTicker
        -- Flatten and convert to Map
        allReturns = concat tickerReturns
    in M.fromList allReturns
    where
        calcTickerReturns :: [StockData] -> [((Day, String), Double)]
        calcTickerReturns [] = []
        calcTickerReturns [_] = []
        calcTickerReturns stocks@(first:_) =
            let sortedByDate = sortOn date stocks
                -- Calculate percent changes between consecutive days
                pairwise = zip sortedByDate (tail sortedByDate)
                returns = map (\(prev, curr) -> 
                        ((date curr, ticker curr), (close curr - close prev) / close prev)) pairwise
            in returns

-- Calculate wallet daily returns using parallelization
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

-- Calculate the covariance matrix for stock returns with improved parallelization
calculateCovarianceMatrix :: [StockData] -> [String] -> IO (V.Vector (V.Vector Double))
calculateCovarianceMatrix stockData sortedTickers = do
    -- Calculate daily returns for all stocks
    let dailyReturns = calculateDailyReturns stockData
    
    -- Get all unique dates in chronological order
    let allDates = nub $ map (fst . fst) $ M.toList dailyReturns
        sortedDates = sortOn id allDates
    
    -- Create a map of ticker -> list of returns ordered by date
    let tickerReturnsMap = M.fromList $ map (\ticker -> 
            let tickerDailyReturns = map (\date -> 
                    M.findWithDefault 0.0 (date, ticker) dailyReturns) sortedDates
            in (ticker, tickerDailyReturns)) sortedTickers
    
    -- Calculate means for each ticker
    let tickerMeans = M.map (\returns -> 
            if null returns then 0 else sum returns / fromIntegral (length returns)) tickerReturnsMap
    
    -- Number of tickers
    let n = length sortedTickers
    
    -- Prepare data for covariance calculation
    let rowData = [(i, sortedTickers !! i, tickerReturnsMap M.! (sortedTickers !! i), 
                   tickerMeans M.! (sortedTickers !! i)) | i <- [0..(n-1)]]
    
    -- Calculate covariance matrix rows in parallel
    -- Use parBuffer for controlled parallelism
    -- Get number of CPU cores to optimize chunk size
    numCores <- getNumCapabilities
    let chunkSize = max 1 (n `div` (numCores * 2))
        
        -- Function to calculate a single row of the covariance matrix
        calcRow (rowIdx, rowTicker, rowReturns, rowMean) = 
            V.generate n $ \colIdx ->
                let colTicker = sortedTickers !! colIdx
                    colReturns = tickerReturnsMap M.! colTicker
                    colMean = tickerMeans M.! colTicker
                    
                    -- Create pairs of corresponding daily returns
                    returnPairs = zip rowReturns colReturns
                    
                    -- Calculate the covariance
                    covSum = sum $ map (\(r1, r2) -> (r1 - rowMean) * (r2 - colMean)) returnPairs
                    
                    -- Divide by n-1 for sample covariance
                    cov = if null returnPairs || length returnPairs <= 1
                          then 0
                          else covSum / fromIntegral (length returnPairs - 1)
                in cov
        
        -- Calculate rows in parallel using parBuffer strategy
        covRows = withStrategy (parBuffer chunkSize rdeepseq) $ map calcRow rowData
    
    -- Construct the covariance matrix
    return $ V.fromList covRows

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

-- Pretty print a wallet with just 3 example stocks and annual return
printWallet :: Wallet -> [String] -> Maybe Double -> IO ()
printWallet wallet tickers mbReturn = do
    putStrLn "Wallet allocation (3 examples):"
    
    -- Get the top 3 holdings by weight
    let nonZeroWeights = [(idx, w) | idx <- [0..V.length wallet - 1], let w = wallet V.! idx, w > 0]
        sortedWeights = take 3 $ sortOn (negate . snd) nonZeroWeights
    
    -- Print the 3 examples
    mapM_ (\(idx, weight) ->
        putStrLn $ "  " ++ tickers !! idx ++ ": " ++ showPercentage weight) sortedWeights
    
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

-- Helper for when keyword
when :: Bool -> IO () -> IO ()
when cond action = if cond then action else return ()
module Simulate
    ( Wallet
    , generateRandomWallets
    , printWallet
    , printWalletWithReturn
    , calculateWalletReturns
    , calculateWalletVolatility
    , printWalletWithReturnAndVolatility
    , calculateWalletReturnsAndVolatilities
    ) where

import System.Random
import Data.List (nub, sortOn, groupBy, elemIndex, unfoldr)
import Data.Function (on)
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Control.DeepSeq (NFData, rnf)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Time (Day)
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

-- Generate n random wallets using parallelization
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

-- NEW: Calculate the covariance matrix for stock returns
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
    
    -- Calculate covariance matrix
    let n = length sortedTickers
        covMatrix = V.generate n $ \i -> 
            let ticker1 = sortedTickers !! i
                returns1 = tickerReturnsMap M.! ticker1
                mean1 = tickerMeans M.! ticker1
            in V.generate n $ \j -> 
                let ticker2 = sortedTickers !! j
                    returns2 = tickerReturnsMap M.! ticker2
                    mean2 = tickerMeans M.! ticker2
                    -- Create pairs of corresponding daily returns
                    returnPairs = zip returns1 returns2
                    -- Calculate the covariance
                    covSum = sum $ map (\(r1, r2) -> (r1 - mean1) * (r2 - mean2)) returnPairs
                    -- Divide by n-1 for sample covariance
                    cov = if null returnPairs || length returnPairs <= 1
                          then 0
                          else covSum / fromIntegral (length returnPairs - 1)
                in cov
    
    return covMatrix

-- NEW: Calculate volatility for a wallet
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

-- NEW: Calculate returns and volatilities for all wallets
calculateWalletReturnsAndVolatilities :: [StockData] -> [Wallet] -> IO [(Wallet, Double, Double)]
calculateWalletReturnsAndVolatilities stockData wallets = do
    -- Extract unique tickers in order
    let allTickers = nub $ map ticker stockData
        sortedTickers = sortOn id allTickers
    
    -- Calculate returns for all wallets
    walletReturns <- calculateWalletReturns stockData wallets
    
    -- Calculate covariance matrix
    covMatrix <- calculateCovarianceMatrix stockData sortedTickers
    
    -- Calculate volatilities for all wallets
    let walletVolatilities = map (\(wallet, ret) -> 
            let vol = calculateWalletVolatility wallet covMatrix
            in (wallet, ret, vol)) walletReturns
    
    -- Using parallelization to compute results
    return $ parMap rdeepseq id walletVolatilities

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

-- NEW: Print a wallet with its return and volatility
printWalletWithReturnAndVolatility :: (Wallet, Double, Double) -> [String] -> IO ()
printWalletWithReturnAndVolatility (wallet, ret, vol) tickers = do
    -- Print basic wallet info and return
    printWallet wallet tickers (Just ret)
    
    -- Print volatility
    putStrLn $ "Annual volatility: " ++ showPercentage vol
    
    -- Print Sharpe ratio (assuming risk-free rate of 0.02)
    let riskFreeRate = 0.02
        sharpeRatio = (ret - riskFreeRate) / vol
    putStrLn $ "Sharpe ratio: " ++ show sharpeRatio
    
    where
        showPercentage :: Double -> String
        showPercentage value = show (value * 100) ++ "%"

-- Helper for when keyword
when :: Bool -> IO () -> IO ()
when cond action = if cond then action else return ()
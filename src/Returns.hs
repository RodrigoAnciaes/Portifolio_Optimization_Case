module Returns
    ( calculateDailyReturns
    , calculateWalletReturn
    , calculateCovarianceMatrix
    ) where

import DataLoader (StockData(..))
import Portfolio (Wallet)
import Data.List (nub, sortOn, groupBy)
import Data.Function (on)
import Control.Parallel.Strategies (parBuffer, rdeepseq, withStrategy)
import Control.Concurrent (getNumCapabilities)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Time (Day)

-- Calculate daily returns from stock data
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

-- Helper for calculating wallet return for a specific date
calculateWalletReturnForDate :: M.Map (Day, String) Double -> [String] -> Wallet -> Day -> Double
calculateWalletReturnForDate returnMap tickers wallet day =
    -- Multiply each stock's weight by its return for the day, and sum
    V.sum $ V.imap (\i weight -> 
        let ticker = tickers !! i
            stockReturn = M.lookup (day, ticker) returnMap
        in case stockReturn of
            Just ret -> weight * ret
            Nothing -> 0) wallet

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
module Optimization
    ( processAllCombinations
    , BestWallet
    ) where

import Portfolio (Wallet, calculateSharpeRatio, generateRandomWallet, calculateWalletVolatility)
import Returns (calculateWalletReturn)
import System.Random (StdGen)
import Control.Monad (foldM, when)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Time (Day)

-- Type for tracking the best wallet found
type BestWallet = (Wallet, Double, Double, Double) -- (wallet, return, volatility, sharpe)

-- Helper function to process all combinations recursively
processAllCombinations :: BestWallet -> StdGen -> Int -> [([Int], [String])] -> Int -> Int -> Int
                        -> M.Map (Day, String) Double -> [String] -> V.Vector (V.Vector Double) -> Double 
                        -> IO BestWallet
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
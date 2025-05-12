module Utilities
    ( binomial
    , showPercentage
    ) where

-- Helper function to calculate binomial coefficient (n choose k) (For calculating combinations)
binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0
    | k == 0 || k == n = 1
    | k > n - k = binomial n (n - k)
    | otherwise = binomial (n - 1) (k - 1) * n `div` k
    
-- Utility for showing percentage values
showPercentage :: Double -> String
showPercentage value = show (value * 100) ++ "%"
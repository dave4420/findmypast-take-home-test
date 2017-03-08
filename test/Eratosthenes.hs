module Eratosthenes (primes) where

-- This is not the most efficient algorithm, but is fairly simple,
-- so hopefully less likely to be buggy

primes :: [Int]
primes = 2 : filter isPrime [3, 5..]

isPrime :: Int -> Bool
isPrime candidate = all coprimeTo smallPrimes
  where
    coprimeTo prime = 0 /= candidate `mod` prime
    smallPrimes = takeWhile (<= root) primes
    root = floor (sqrt (fromIntegral candidate :: Double))


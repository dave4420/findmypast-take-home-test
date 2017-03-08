{-# LANGUAGE LambdaCase #-}

module Atkin (primes) where

-- implementation of the Sieve of Atkin
-- <https://en.wikipedia.org/wiki/Sieve_of_Atkin>

import qualified Data.Map as M

primes :: [Int]
primes = 2 : 3 : 5 : nextPrimes [7, 9..]

nextPrimes :: [Int] -> [Int]
nextPrimes = \case
    [] -> []
    x : xs ->
        let emit = x : filter (not . isMultipleOfSquare) (nextPrimes xs)
            isMultipleOfSquare y = 0 == y `mod` square
            square = x * x
            --DAVE: filtering out using a priority queue would be more efficient
        in case whichCase x of
            Just CaseOne   | odd (countCaseOneSolutions x)   -> emit
            Just CaseTwo   | odd (countCaseTwoSolutions x)   -> emit
            Just CaseThree | odd (countCaseThreeSolutions x) -> emit
            _                                                -> nextPrimes xs

data Case = CaseOne | CaseTwo | CaseThree

cases :: M.Map Int Case
cases = M.fromList
    [ (1, CaseOne)
    , (13, CaseOne)
    , (17, CaseOne)
    , (29, CaseOne)
    , (37, CaseOne)
    , (41, CaseOne)
    , (49, CaseOne)
    , (53, CaseOne)
    , (7, CaseTwo)
    , (19, CaseTwo)
    , (31, CaseTwo)
    , (43, CaseTwo)
    , (11, CaseThree)
    , (23, CaseThree)
    , (47, CaseThree)
    , (59, CaseThree)
    ]

whichCase :: Int -> Maybe Case
whichCase x = M.lookup (x `mod` 60) cases

-- number of solutions to 4x² + y² = n
countCaseOneSolutions :: Int -> Int
countCaseOneSolutions = undefined --DAVE

-- number of solutions to 3x² + y² = n
countCaseTwoSolutions :: Int -> Int
countCaseTwoSolutions = undefined --DAVE

-- number of solutions to 3x² − y² = n, x > y
countCaseThreeSolutions :: Int -> Int
countCaseThreeSolutions = undefined --DAVE

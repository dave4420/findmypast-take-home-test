{-# LANGUAGE LambdaCase #-}

module Atkin (primes) where

-- implementation of the Sieve of Atkin
-- <https://en.wikipedia.org/wiki/Sieve_of_Atkin>

import           Control.Monad        (guard)
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Min as PQ

primes :: [Int]
primes = 2 : 3 : 5 : nextPrimes PQ.empty [7, 9..]

nextPrimes :: PQ.MinPQueue Int Int -> [Int] -> [Int]
nextPrimes queue = \case
    [] -> []
    x : xs -> case PQ.minViewWithKey queue of
        Just ((multipleSquare, oneSquare), queue')
            | multipleSquare == x
                -> nextPrimes
                    (PQ.insert (multipleSquare + oneSquare) oneSquare queue')
                    xs

            | multipleSquare < x
                -> nextPrimes
                    (PQ.insert (multipleSquare + oneSquare) oneSquare queue')
                    (x : xs)

        _ ->
            let emit = x : nextPrimes (PQ.insert square square queue) xs
                square = x * x
                skip = nextPrimes queue xs
            in case whichCase x of
                Just CaseOne   | odd (countCaseOneSolutions x)   -> emit
                Just CaseTwo   | odd (countCaseTwoSolutions x)   -> emit
                Just CaseThree | odd (countCaseThreeSolutions x) -> emit
                _                                                -> skip

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

--DAVE: investigate whether there's a more efficient algorithm for solving these
--      equations

--DAVE: intSqrt + guard can probably be combined for greater efficiency

-- number of solutions to 4x² + y² = n
countCaseOneSolutions :: Int -> Int
countCaseOneSolutions n = length $ do
    x <- [0 .. intSqrt (n `div` 4)]
    let y = intSqrt (n - 4 * x * x)
    guard (4 * x * x + y * y == n)
    pure (x, y)

-- number of solutions to 3x² + y² = n
countCaseTwoSolutions :: Int -> Int
countCaseTwoSolutions n = length $ do
    x <- [0 .. intSqrt (n `div` 3)]
    let y = intSqrt (n - 3 * x * x)
    guard (3 * x * x + y * y == n)
    pure (x, y)

-- number of solutions to 3x² − y² = n, x > y
countCaseThreeSolutions :: Int -> Int
countCaseThreeSolutions n = length $ do
    x <- [0 .. intSqrt (n `div` 2)]
    let y = intSqrt (3 * x * x - n)
    guard (3 * x * x - y * y == n)
    pure (x, y)

intSqrt :: Int -> Int
intSqrt z = floor (sqrt (fromIntegral z :: Double))

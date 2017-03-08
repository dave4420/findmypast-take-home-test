{-# LANGUAGE LambdaCase #-}

import qualified Eratosthenes

import qualified Test.Tasty            as Tst
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tst.defaultMain tests

tests :: Tst.TestTree
tests = Tst.testGroup "tests"
    [ Tst.testGroup "Sieve of Eratosthenes"
        (basicPrimalityTests (take 1000 Eratosthenes.primes))
    ]

basicPrimalityTests :: [Int] -> [Tst.TestTree]
basicPrimalityTests primes
    = [ QC.testProperty "primes are produced in strictly ascending order"
            (prop_strictlyAscending primes)
      , QC.testProperty "primes are all coprime to each other"
            (prop_allCoprime primes)
      ]

prop_strictlyAscending :: [Int] -> QC.Property
prop_strictlyAscending = \case
    x : ys @ (y : _)
        | x < y
            -> prop_strictlyAscending ys
        | otherwise
            -> QC.counterexample (unwords [show x , "preceded", show y]) False
    _ -> QC.property True

prop_allCoprime :: [Int] -> QC.Property
prop_allCoprime = \case
    [] -> QC.property True
    x : xs -> case filter (coprime x) xs of
        []    -> prop_allCoprime xs
        y : _ -> QC.counterexample
                    (unwords [show x, "and", show y, "are not coprime"])
                    False

coprime :: Int -> Int -> Bool
coprime x y = 1 == gcd x y

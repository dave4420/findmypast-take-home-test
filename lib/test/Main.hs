{-# LANGUAGE LambdaCase #-}

import qualified Atkin

import qualified Eratosthenes

import qualified Test.Tasty            as Tst
import           Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tst.defaultMain tests

tests :: Tst.TestTree
tests = Tst.testGroup "tests"
    [ Tst.testGroup "Sieve of Eratosthenes"
        (basicPrimalityTests (take eratosthenesCount Eratosthenes.primes))
    , Tst.testGroup "Sieve of Atkin"
        ( basicPrimalityTests (take atkinCount Atkin.primes)
        ++ consistencyTests Atkin.primes
        )
    ]

eratosthenesCount :: Int
eratosthenesCount = 1000

atkinCount :: Int
atkinCount = 10000

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
    x : xs -> case filter (not . coprime x) xs of
        []    -> prop_allCoprime xs
        y : _ -> QC.counterexample
                    (unwords [show x, "and", show y, "are not coprime"])
                    False

coprime :: Int -> Int -> Bool
coprime x y = 1 == gcd x y

consistencyTests :: [Int] -> [Tst.TestTree]
consistencyTests primesUnderTest'
    = [ QC.testProperty "is consistent with the Sieve of Eratosthenes"
            (primesUnderTest === knownGoodPrimes)
      ]
  where
    primesUnderTest = take eratosthenesCount primesUnderTest'
    knownGoodPrimes = take eratosthenesCount Eratosthenes.primes

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
      , QC.testProperty "primes contains all factors of a random number"
            (prop_factorsAreInPrimes primes)
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

prop_factorsAreInPrimes :: [Int] -> QC.Gen QC.Property
prop_factorsAreInPrimes primes0
    = go primes0 <$> QC.choose (maxPrime `div` 2, maxPrime)
    -- any prime in the list will be a factor of the chosen n with probability
    -- of approximately 1/p: this test is much more likely to notice when a
    -- small prime is missing than it is when a large prime is missing
  where
    maxPrime = last primes0

    go :: [Int] -> Int -> QC.Property
    go _ 1               = QC.counterexample "passes" True
    go [] n              = (QC.counterexample . unwords)
                            [ show n
                            , "has no prime factors; reached end of list"
                            ]
                            False
    go (p : ps) n
        | n < p          = (QC.counterexample . unwords)
                            [ show n
                            , "has no prime factors; reached"
                            , show p
                            ]
                            False
        | remainder == 0 = go (p : ps) quotient
        | otherwise      = go ps n
      where
        (quotient, remainder) = n `divMod` p

consistencyTests :: [Int] -> [Tst.TestTree]
consistencyTests primesUnderTest'
    = [ QC.testProperty "is consistent with the Sieve of Eratosthenes"
            (primesUnderTest === knownGoodPrimes)
      ]
  where
    primesUnderTest = take eratosthenesCount primesUnderTest'
    knownGoodPrimes = take eratosthenesCount Eratosthenes.primes

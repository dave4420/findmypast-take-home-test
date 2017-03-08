import qualified Test.Tasty as Tst

main :: IO ()
main = Tst.defaultMain tests

tests :: Tst.TestTree
tests = Tst.testGroup "tests" []  --DAVE: write tests

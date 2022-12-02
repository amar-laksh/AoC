import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Data.List
import Data.Maybe
import Day1 as D1
import System.IO.Silently
import Test.SmallCheck.Series (NonEmpty (NonEmpty), NonNegative (NonNegative), list)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [task1Test]

task1Test :: TestTree
task1Test =
  testGroup
    "*** Day 1 ***"
    [ SC.testProperty "Test (Pure)" $
        \(msg :: String) -> T1.helloWorldPure msg == "hello world: " ++ msg,
      SC.testProperty "Test (Monadic)" $
        \(msg :: String) -> SC.monadic $ do
          (systemPutStrLn, _) <- capture (putStrLn ("hello world: " ++ msg))
          (myPutStrLn, _) <- capture (T1.helloWorldMonadic msg)
          return $ systemPutStrLn == myPutStrLn
    ]

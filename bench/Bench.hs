import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ bgroup
        "2022 "
        [ bcompare "Days" $ bench "day1" $ nfIO D1.day1,
          bench "day2" $ nfIO D2.day2,
          bench "day3" $ nfIO D3.day3,
          bench "day4" $ nfIO D4.day4,
          bench "day5" $ nfIO D5.day5,
          bench "day6" $ nfIO D6.day6
        ]
    ]

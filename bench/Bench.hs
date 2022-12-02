import Task1 as T1
import Test.Tasty.Bench

msgs :: [Char]
msgs = ['a' .. 'z'] :: [Char]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Benchmark (Pure)"
        [ bcompare "a to z characters" $ bench "T1.helloWorldPure" $ nf T1.helloWorldPure msgs,
          bcompare "a to z characters" $ bench "raw ++" $ nf ("hello world: " ++) msgs
        ],
      bgroup
        "Benchmark (Monadic)"
        [ bcompare "a to z characters" $ bench "T1.helloWorldMonadic" $ nfIO (T1.helloWorldMonadic msgs),
          bcompare "a to z characters" $ bench "putStrLn" $ nfIO (putStrLn ("hello world: " ++ msgs))
        ]
    ]

module Lib
    ( someFunc
      , doubleFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleFunc :: Int -> Int
doubleFunc x = x + x

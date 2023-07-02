module Lib
    ( someFunc
    ) where


funcy :: String -> Int
funcy x = read x :: Int

someFunc :: IO ()
someFunc = do
    
   putStrLn $ show 

{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Lib


foreign import ccall "addThreeInts" c_addThreeInts
    :: Int -> Int -> Int -> Int

addThreeInts :: Int -> Int -> Int -> Int
addThreeInts a b c = c_addThreeInts a b c


main :: IO ()
main = do
   putStrLn $  ("Trying this out" ++)  $  show $ addThreeInts 1 2 3 
   
   startApp

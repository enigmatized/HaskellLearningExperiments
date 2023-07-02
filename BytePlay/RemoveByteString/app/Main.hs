{-# LANGUAGE BangPatterns         #-}

module Main (main) where

import Control.DeepSeq
import Lib
import           Data.Bits
import qualified  Data.ByteString.Short as SBS
import Data.Word
import qualified Data.ByteString           as B

main :: IO ()
main = do
    someFunc
    --someFunc2
    putStrLn "Some someFunc magic above"
    let !x = force $   my32LengthShortByteString
    putStrLn $ show $  "Showing what bytesToWord64 which should be a word64 representation"
    putStrLn $ show $  bytesToWord64 $ (SBS.fromShort getShort0 :: B.ByteString)
    putStrLn $ show $  "Showing what bytesToWord8 which should be a word8 representation"
    putStrLn $ show $  bytesToWord8 $ (SBS.fromShort getShort0 :: B.ByteString)
    putStrLn $ show $  "Showing what bytesToWord8encodeed which should be a word8 representation"
    putStrLn $ show $  bytesToWord8'B16 $ (SBS.fromShort getShort0 :: B.ByteString)
    putStrLn $ show $  "Showing what bytesToWord16 which should be a word16 representation"
    putStrLn $ show $  bytesToWord16 $ (SBS.fromShort getShort0 :: B.ByteString)
    putStrLn "---------------"
    byteArrayToWords' getShort0
    putStrLn "---------------"
--putStrLn $ show $ getByte 1 ("22" :: SBS.ShortByteString)
    putStrLn "Garrett is about to print my32LengthShortByteString"
    putStrLn $ show $ x
    putStrLn $ show $ "What is length byteArray function produce"
    putStrLn $ show $ giveMeSize x
    printBytes'
    putStrLn $ show $ "Garrett is showing what ChatGPT can do"
    putOutTest
    putStrLn $ show $ "\n Printing foo\n"
    getFoo
    putStrLn $ show $ "What the fuck is this"
    putStrLn $ show $ x
    putStrLn $ show $ "what does a byteArray# look like printed out?"
    --putStrLn $ show $  getByteArray x
    putStrLn $ show $ "What length will this print"
    --putStrLn $ show $  whatIsThis' $ getByteArray
    putStrLn $ ("This is min bound : " ++ ) $ show $ (minBound ::Int)
    putStrLn $ show $ createIdunno' x

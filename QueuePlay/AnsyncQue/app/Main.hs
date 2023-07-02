{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Lib
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM


-- Create a new empty queue
newQueue :: IO (TQueue a)
newQueue = newTQueueIO

-- Add an element to the end of the queue
enqueue :: TQueue a -> a -> IO ()
enqueue queue item = atomically $ writeTQueue queue item

-- Remove and return the first element from the queue
dequeue :: TQueue a -> IO a
dequeue queue = atomically $ readTQueue queue



main :: IO ()
main = do
   someFunc
   queue <- newQueue
   producerThreadId <- forkIO $ replicateM_ 10  $ enqueue queue "item"
   consumerThreadId <- forkIO $ replicateM_ 10 $ dequeue queue >>= print
   --mapM_ (\t -> threadDelay 1000000 >> killThread t) [producerThreadId, consumerThreadId]

   !_  <- replicateM_ 10 $ forkIO $ do putStrLn "Did we get here" >> enqueue queue "item"
   !_  <- replicateM_ 10 $ forkIO $ dequeue queue >>= print
   putStrLn "Did we do it boys?"

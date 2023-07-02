{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Conduit
import Data.Conduit.List (sourceList )


-- 
-- Create an instance of a Conduit
-- Create an Monad instance
-- Create an ConduitT instance
-- Create an Overlapping ConduitT instance



data Dog a = Dog  {age :: a, size :: Int }


instance (Num a, Show a) => Show (Dog a) where
	show (Dog x b) = "Dog is age" ++ (show x) ++ " and size of " ++ (show b)

--instance (Num a, Show a) => Conduit (Dog a) IO (Dog a)  where
--	conduit (Dog x, a) = do
--		let processedData = Dog (a * 2)
--    		yield processedData

--instance Monad m => ConduiT 



class ExampleClass a where
    exampleMethod :: a -> String

instance ExampleClass Int where
    exampleMethod _ = "Instance for Int"

ambiguousExample ::  String
ambiguousExample = exampleMethod (42 ::Int)

main :: IO ()
main = do
	someFunc
	let numbers = [1, 2, 3, 4, 5]
  	runConduitRes $
    		sourceList numbers
    		.| mapC (* 2)
    		.| sinkList
    		>>= liftIO . print

module Main (main) where



import Lib
import Data.Foldable
--import Monad.Control
import qualified Control.Monad.State                   as State



main :: IO ()
main = do
    
    -- Variable setting up
    let pp        = putStrLn . show
    let ppMe iSay = putStrLn . (iSay ++) 
    let og_state  = ([1,2,3,4], 1) 
    

       
    ppMe "Print og_out " $ show og_state 

    let what_is_this = flip State.execState  (og_state, 2) $ do
         weGot_it <- State.get
         weGot_2  <- State.gets fst 
         let funny = fst $ fst weGot_it
         State.put (( (map (2 + ) funny) , 666), 3)
         return [23]
    
    ppMe "print out new state " $ show what_is_this

    
    putStrLn $ ("Garrett thinks this will print a list of list" ++) (show what_is_this)
    let holder_of_thunk = for_ [1..5] $ (\x -> pp x)
    putStrLn " Garrett Got Here First"
    whatIsThis <-holder_of_thunk
    putStrLn $ ("Amazing nature" ++) (show whatIsThis)   
    someFunc

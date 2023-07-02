{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Lib
import           GHC.Conc
import           Control.Concurrent
import           Control.Monad
import           Control.Concurrent.Supervisor         hiding (length)
import           Control.Concurrent.SupervisorInternal hiding (length)
import           Control.Concurrent.Async
import           Data.Default
--import           Graphics.Matplotlib
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Monad.IO.Class
import GHC.Float


signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]


testing_Out :: IO ()
testing_Out = toFile def "example1_big_.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal [0,(0.5)..400]])
    --plot (points "am points" (signal [0,7..400]))


testing_Outi'' :: [PandasRow] ->  IO ()
testing_Outi'' pandasDf  = toFile def "example6_big_.png" $ do
    layout_title .= "Garrett Modulation"
    setColors [opaque blue]
    --plot (line "am" [signal [0,(0.5)..400]])
    --putStrLn $ "length of pandasRow" ++ (show $ length pandasDf)
    
    --plot (points "am points"  [(1,1),(2,2), (3,3)])--zip [ open  x  | x  <-  pandasDf ] [0 .. (length pandasDf)]  )
    
    plot (points "am points"  (zip [float2Double $  open  x  | x  <-  pandasDf ] [0 .. (length pandasDf)]  ))
    --let sublist_ = take 10 pandasDf
    --putStrLn $ show (zip [float2Double $  open  x  | x  <-  sublist_  ] [0 .. (length sublist_)] )

printForSeconds :: IO ()
printForSeconds = threadDelay 20 >> putStrLn "Hi!" >> printForSeconds


printThreadStatus :: ThreadId -> IO ()
printThreadStatus id = do
   threadId' <- threadStatus id
   case threadId' of
       ThreadDied    -> putStrLn "!!!   We killed the thread"
       --ThreadRunning -> putStrLn "!!!  Thread Running"
       --ThreadFinished -> putStrLn "!!! Not sure when this should occur"
       --_              -> putStrLn "When does this ever happen?"
       _ -> putStrLn " not died" >> threadDelay 2 >> printThreadStatus id


--SO I am forking a thread, that forks a thread
--But when I do that, it doesn't work
--

main :: IO ()
main = do 
   --testing_Out
   best <- getFile
   --let best = []
   --putStrLn $ show best 
   putStrLn $ "length of pandasRow" ++ (show $ length best)
   let sublist_ =  best 
   putStrLn $ ("what does sublist_ look like" ++ ) $ show sublist_
   putStrLn $ ( "yo_ you" ++ )   $ show (zip [float2Double $  open  x  | x  <-  sublist_  ] [0 .. (length sublist_)] )   
   --let xxx = 
   testing_Outi''  best
   putStrLn "_________ csv test ________" 
   --test
   putStrLn "----Conduit test above---" 
   --threadid <- forkIO $ printForSeconds
   --forM_ [1..100]  $ \_ -> forkIO $ printThreadStatus threadid 
   --putStrLn $ "Thread ID " ++ (show threadid)

   -- onscreen $ let b = bar [21..23] in
   --   b [56,57,56] @@ [o2 "color" "#4C78A8"] 
   -- % b [26,24,25] @@ [o2 "color" "#F58518"] 
   -- % b [16,15,14] @@ [o2 "color" "#E45756"]
   --onscreen $ contourF (\a b -> sin (a*pi/180.0) + cos (b*pi/180.0)) (-100) 100 (-200) 200 10 
   --onscreen $ contourF (\a b -> sin (a*pi/180.0) + cos (degreesRadians b)) (-100) 100 (-200) 200 10


   let runYourSupervisorWithStaticChildren = do
        Actor svQ svAction <- newActor $ newSupervisor  OneForAll   def  [ newChildSpec Permanent printForSeconds]
        async svAction
   runYourSupervisorWithStaticChildren 
   putStrLn "yay!"
   
   --onscreen $ plot [1,2,3,4,5,6] [1,3,2,5,2] @@ [o1 "go-", o2 "linewidth" 2]
   --onscreen $
   --     subplots @@ [o2 "nrows" 2, o2 "ncols" 2]
   --     % setSubplot 0 % plotMapLinear (**0) (-2) 2 100
   --     % setSubplot 1 % plotMapLinear (**1) (-2) 2 100
   --     % setSubplot 2 % plotMapLinear (**2) (-2) 2 100
   --     % setSubplot 3 % plotMapLinear (**3) (-2) 2 100

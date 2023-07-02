module Main (main) where

import           Data.Map
import Lib
import           Control.Monad.Reader
import           Control.Monad.Trans.State

data R = R {
       cc :: String
}

-- Holly shit this is a type
-- What does that imply?

type SSS = StateT [String] (Reader R)
-- SO How do I add to StateT
-- How do I get contets
-- Then leave it without SSS
--Hmm Maybe keeping stateT a Map instead of a string I think this might involve anot

type SSS2 = StateT (Map String String) (Reader R)



-- So for type checker
--
-- contract A {
--    int a = 3;
   
   --constructor () {
      -- a+=1
      -- int a = b;
      -- How do I know a?
   --}

-- }

testLayer1 :: String
testLayer1 = do
   --gAdd
   let r = R "eee"
    in runReader (evalStateT getSSS []) r 


produceReader :: Reader R String
produceReader = funcLayer3 3
-- produceReader2 :: Reader R String
-- produceReader2 = 


addTo :: Int -> Reader R String
addTo x = if x > 0 then do
      cc <- asks cc
      
      let xcc = cc ++ ( show cc)
      let r = R xcc
      let xxx = runReader (addTo $ x - 1)  r
      let rr = R xxx
      let xxxx = runReader (addTo $ x - 1)  rr
      --putStr "HOw many time can I get here?"
      addTo $ x - 1
   else do
      el <- asks cc
      pure $ el ++ "Finished"


   --pure $ "produce Reader"

-- produceSSS :: StateT [String] (Reader R)
-- produceSSS = pure $ [" asd"]

getSSS :: SSS String
getSSS = do
    --disFunct
    --pushLocalVariable "That would be cool if this worked"
    --pure $ "1-String-\n"
    ev <- (put ["Will this work"] ) 
    pure $  "1-String-\n"

extract1SSS :: Reader R [String]
extract1SSS = execStateT (getSSS) ["Testing123344"]

pushLocalVariable :: String -> SSS ()
pushLocalVariable x  = modify $ \y -> y 



--
--This function produces
--  produceReader1Will work for food2
evalStateTs :: SSS String
evalStateTs = do
   helpful <- asks cc
   execStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   pure $ "produceReader" ++ helpful  ++ (head ev2)

--
--This function produces
--  produceReader1Will work for foodWill work for food2
--  I believe this is because I am using execStateT :: Monad m => StateT s m a -> s -> m s
evalStateTs2 :: SSS String
evalStateTs2 = do
   helpful <- asks cc
   ev1 <- execStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   pure $ "produceReader" ++ helpful ++ (head ev1)  ++ (head ev2)




--This function produces
--  produceReader1Will work for foodWill work for food2
--  evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateTs3 :: SSS String
evalStateTs3 = do
   helpful <- asks cc
   evalStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   pure $ "produceReader" ++ helpful  ++ (head ev2)

evalStateTs4 :: SSS String
evalStateTs4 = do
   helpful <- asks cc
   evalStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   whatDoesTHis <- get --THis is from Eval State-- Intresting
   

   pure $ "produceReader" ++ helpful  ++ (concat whatDoesTHis) ++  (concat whatDoesTHis)



evalStateTs5 :: SSS String
evalStateTs5 = do
   helpful <- asks cc
   evalStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   whatDoesTHis <- get --THis is from Eval State-- Intresting
   put ["We are deeper in state"]
   whatDoesTHis2 <- get

   pure $ "produceReader" ++ helpful  ++ (concat whatDoesTHis) ++  (concat whatDoesTHis) ++ (concat whatDoesTHis2)

evalStateTs6 :: SSS String
evalStateTs6 = do
   helpful <- asks cc
   evalStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   whatDoesTHis <- get --THis is from Eval State-- Intresting
   put ["We are out of state at stage 6"]
   modify (++["deeper V3"])
   modify (++["deeper V3"])
   modify (++["deeper V3"])
   whatDoesTHis2 <- get
   

   pure $ "produceReader" ++ helpful  ++ (concat whatDoesTHis) ++  (concat whatDoesTHis) ++ (concat whatDoesTHis2)


evalStateTs7 :: SSS String
evalStateTs7 = do
   helpful <- asks cc
   evalStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   whatDoesTHis <- get --THis is from Eval State-- Intresting
   put ["We are out of state at stage 6"]
   modify (++["deeper V3"])
   modify (++["deeper V3"])
   modify (++["deeper V3"])
   canIaddToStateInsideState
   whatDoesTHis2 <- get
   

   pure $ "produceReader" ++ helpful  ++ (concat whatDoesTHis) ++  (concat whatDoesTHis) ++ (concat whatDoesTHis2)


canIaddToStateInsideState :: SSS String
canIaddToStateInsideState = do 
   (modify (++ ["I CAN MODIFY "])) 
   pure $ "String"
-- testLayer2 :: String
-- testLayer2 = runReader ggSSS (R "asd")

-- testLayer2 :: String
-- testLayer2 =  do
--    runReader $ snd $snd (runStateT (mapM (\x -> state $ \s -> do  (x,s+x)) ["a".."aa"]) "a")-- getSSS)

-- funcLayer2 :: Int -> String
-- funcLayer2 = funcLayer3

funcLayer3 :: Int -> Reader R String
funcLayer3 x = do
   el <- asks cc
   pure $ el ++ "Holly shit, end" ++ (show x)


evalStateTs8 :: SSS2 String
evalStateTs8 = do
   helpful <- asks cc
   evalStateT (modify (++ ["Will this work"]) ) ["Will work for food"]
   ev2 <- execStateT (modify (++ ["22222"])) ["Will work for food2"]
   whatDoesTHis <- get --THis is from Eval State-- Intresting
   -- put ["We are out of state at stage 6"]
   -- modify ()
   -- modify ()
   -- modify ()
   canIaddToStateInsideState
   whatDoesTHis2 <- get
   pure $ "Some BS"


main =  do 
   putStr testLayer1
   -- Okay so the below works because runReader 
   -- runReader:: Reader r a
   --             -> r   -- An initial envirment 
   --             -> a
   -- So producer reader Makes an (Reader R a) and 
   -- r is a R.... That's why its data type is an R
   -- But how are we able to Grow R in Typechecker and Optmizer? ->? Still don't know this
   let r = R "Will this do anything"
   putStr $ runReader produceReader  r
   putStr "\n\n\n"
   -- Below is a test how I can keep adding to R state
   -- I think R state is set?
   -- I think I can push variables to S state?
   let rr = R "1"
   putStr $ runReader (addTo 4)  rr

   putStr $ "\n\n\nTest 4\n"
   --- 
   let what = runReader (evalStateT evalStateTs ["BS example"]) rr
   putStr what
   putStr $ "\n v2\n"
   let what2 = runReader (evalStateT evalStateTs2 ["BS example"]) rr
   putStr what2
   putStr $ "\n v3\n"
   let what3 = runReader (evalStateT evalStateTs3 ["BS example"]) rr
   putStr what3
   putStr $ "\n v4\n"
   let what4 = runReader (evalStateT evalStateTs4 ["xBS examplex"]) rr
   putStr what4

   putStr $ "\n v5\n"
   let what5 = runReader (evalStateT evalStateTs5 ["xBS examplex"]) rr
   putStr what5


   putStr $ "\n v6\n"
   let what6 = runReader (evalStateT evalStateTs6 ["xBS examplex"]) rr
   putStr what6

   putStr $ "\n v7\n"
   let what7 = runReader (evalStateT evalStateTs7 ["xBS examplex"]) rr
   putStr what7


------------------------------------------------------------------
---- SSS2 = StateT (Map ) (Reader R)
-- Add to map
-- And remove from map
-- Depending on context
------------------------------------------------------------------
   putStr $ "\n v8\n"
   let what8 = runReader (evalStateT evalStateTs8 ["xBS examplex"]) rr
   putStr what8


   --putStr testLayer2
   --runStateT (code [1.. 4]) return ()


-- gS :: [String]
-- gS = (evalStateT (get) [])


--ggSSSS  :: Reader R String
--ggSSSS = evalStateT disFunct ["yes"]

--exprV :: Reader

--Oh so the pattern is
--Wrap everything in SSS
--Then pull out and return as a Reader
--so (sss) -> 


   -- putStr testLayer2
   -- putStr (show gS)
   -- putStr (show pS) 
--putStr testLayer2
   -- someFunc


 
-- disFunct :: SSS ()
-- disFunct = put ["asd"]



-- pS :: [String]
-- pS = do  
--     let temp = []
--     (evalStateT get temp)



-- gAdd :: String
-- gAdd = "1+2"


--Lessons Leanrt
--1. What is evalStateT
--2. What is difference between state and stateT
--3. How do I add variables to this?
--4. What is Data.Functor.Identity
--5. Wait what the fuck is the difference between Data and Type in haskell
--6. 




-- How do I produce a function that produces a `Reader a` ?

-- ```hs
-- import           Control.Monad.Reader

-- data R = R {
--        cc :: String
-- }


-- ```
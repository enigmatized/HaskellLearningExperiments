

module Lib
    ( someFunc,
      test,
      getFile,
      PandasRow,
      open,
    ) where



import Prelude hiding (filter)
import qualified Data.ByteString.Lazy as BL
--import Data.Csv -- this is from the Cassava library
import qualified Data.Vector as V
import System.Directory (doesFileExist)

import Data.Conduit
import Data.Conduit.Binary  hiding (head, take, drop)
import Data.CSV.Conduit
import Data.Text (Text)
import Text.CSV
import Control.Monad
import Control.Concurrent hiding (yield)



data PandasRow = PandasRow { 
	time :: String,
        open :: Float,
        high :: Float,
        low  :: Float,
        close :: Float,
        volume :: Int,
        volume_Ma :: Float       

} deriving (Show) 





myProcessor :: Monad m => Conduit (Row Text) m (Row Text)
myProcessor = awaitForever $ yield

-- Let's simply stream from a file, parse the CSV, reserialize it
-- and push back into another file.
test :: IO ()
test = runResourceT $ 
  sourceFile "test/BigFile.csv" $= 
  intoCSV defCSVSettings $=
  myProcessor $=
  fromCSV defCSVSettings $$
  sinkFile "test/BigFileOut.csv"



getFile :: IO [PandasRow]
getFile =  do

    putStrLn "just eneteed getFile"
    recOfAUD <- parseCSVFromFile "Backtesting_data/AUD_USD60.csv"
    --forM_ recOfAUD $ \x -> forkIO $ putStrLn $ show x
    forM_ recOfAUD $ \x -> putStrLn $ show x
    
    putStrLn "Just read from file"
    let recOfAUD' = case recOfAUD of Right xxxx -> xxxx; _ -> [];
    putStrLn $ ("Just got rightCHart" ++)  $ show $ take 3 recOfAUD' 
    
    let recOfAUD'' = drop 1  recOfAUD'
    --let toString''' field''' = field''' :: Field -> String  
    
    --let [a, b, c, d, e, f, g] =  head recOfAUD'
    
    putStrLn "Print Did we do it"
    
    
    return $ map (\[a,b,c,d,e,f,g]-> (PandasRow a (read b) (read c) (read d) (read e) (read f) (read g)))  $ recOfAUD''
    --return $ case x of (x:xs) -> Just (x:xs); _ -> Nothing;
    --return x
 

someFunc :: IO ()
someFunc = putStrLn "someFunc"

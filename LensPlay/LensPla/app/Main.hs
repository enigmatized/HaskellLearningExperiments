
module Main (main) where
import Control.Lens
import Data.Map
import Lib

data CC = CodeCollection {
   _asss ::  Map String (Int)
  }




main :: IO ()
main = someFunc

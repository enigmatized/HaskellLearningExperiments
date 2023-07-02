{-# LANGUAGE DeriveGeneric         #-}
module Lib
    ( someFunc
    ) where


import Test.QuickCheck
import Control.Monad

import qualified Data.Map                                     as M

import qualified Generic.Random                     as GR
import GHC.Generics
import Debug.Trace
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import qualified Data.Text as T


------------
--TYPES
------------
--


genVarIdName :: Gen String
genVarIdName = vectorOf 5 $ elements ['0'..'9']

data Types = String | Int deriving (Show, Generic, Eq)

instance Arbitrary Types where 
   arbitrary = GR.genericArbitrary GR.uniform

data Contract = Contract {
   _storageDefs :: M.Map String (VarDeclF)
   } deriving (Generic, Show, Eq)


data Colour = Red | Blue | Green
    deriving (Show, Generic, Eq)

data VarDeclF = VarDeclF {
   _color :: Colour
   , _isPublic :: Bool
   , _literal :: Types
   } deriving (Generic, Show, Eq)

-------
--Arbitarty
---------

instance Arbitrary Contract where  
  arbitrary = GR.genericArbitrary GR.uniform


instance Arbitrary Colour where
   arbitrary = oneof
      [return Red, return Blue, return Green]

--instance Arbitary VarDecl a
instance Arbitrary VarDeclF  where
  arbitrary = GR.genericArbitrary GR.uniform

data Tree = Leaf Int | Branch Tree Tree
    deriving Show


data TableName = 
    IndexTableName
      { itOrganization :: T.Text
      , itApplication  :: T.Text
      , itContractName :: T.Text
      }

--- Some generator Functions I Built
--
--


genCourse :: String -> Gen String
genCourse ns = do
  ls <- listOf1 $ elements ['a'..'z']
  return (ls ++ "-" ++ ns)

genVarName :: Gen String
genVarName = do
  date <- genVarIdName
  dates <- listOf1 $ frequency [(1, return date), (3, genVarIdName)]
  courses <- traverse genCourse dates
  return $ unwords courses

--tree = oneof [liftM Leaf (choose (1,1000)), liftM2 Branch tree tree]

tree = sized tree'
tree' 0 = liftM Leaf arbitrary
tree' n | n>0 =
        oneof [liftM Leaf arbitrary,
               liftM2 Branch subtree subtree]
  where subtree = tree' (n `div` 2)


getRanCol :: Gen Colour
getRanCol = oneof [return Blue, return Green]

colorProducer :: Colour -> Int
colorProducer Red   = 1
colorProducer Blue  = 2
colorProducer Green = 3


reverseColorProdcuer :: Int -> Colour
reverseColorProdcuer 1 = Red
reverseColorProdcuer 2 = Blue
reverseColorProdcuer 3 = Green

---------
--Properties
----------
prop_color :: [Colour] -> Bool
prop_color x = (map(\col -> colorProducer col)  x) == (map (\col -> colorProducer $ reverseColorProdcuer   $ colorProducer col) x)



prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys


prop_id :: [VarDeclF] -> Bool
prop_id x  = trace ( show x) (all (True ==) $  map (\same -> (reverseColorProdcuer  (colorProducer $ _color same)) == (_color same)) x)

prop_id_cc :: [Contract] -> Bool
prop_id_cc cc = trace (show cc)  $ (map show cc ) == (map show cc)

prop_id_cc2 :: [Contract] -> Bool
prop_id_cc2 cc = trace (show cc)  $ cc == cc
----
--QuickCheck Call
-----
moo = quickCheck prop_color


---Main call belows

someFunc :: IO ()
someFunc = do
   -- quickCheck prop_id
   --quickCheck prop_revappi
   quickCheck prop_id_cc2 --Large print out
   --show 


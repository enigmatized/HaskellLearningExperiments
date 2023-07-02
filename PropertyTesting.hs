import Test.QuickCheck
import Control.Monad
import qualified Generic.Random                     as GR


-- Sept 13th few things
-- 1. I need to make Aribitary of recursive types
-- But also nest Arbirary types
-- I think Below covers this
-- ColdeCollection has Contract--> We need arbitary contracts

--Okay for a test lets make a new type
--That has Colour for its deal.
--Then Test that
--
--



data Colour = Red | Blue | Green
    deriving Show

data VarDeclF = VarDeclF {
   _color :: Colour
   --, _isPublic :: Bool
   }

instance Arbitrary Colour where
   arbitrary = oneof
      [return Red, return Blue, return Green]

--instance Arbitary VarDecl a
instance Arbitrary VarDeclF  where
  arbitrary = GR.genericArbitrary GR.uniform

data Tree = Leaf Int | Branch Tree Tree
    deriving Show

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

prop_color :: [Colour] -> Bool
prop_color x = (map(\col -> colorProducer col)  x) == (map (\col -> colorProducer $ reverseColorProdcuer   $ colorProducer col) x)


prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

main = quickCheck prop_revapp

moo = quickCheck prop_color

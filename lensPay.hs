{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Prelude hiding (foldr)
import Data.Map as M
import Control.Lens.At
--import Data.Aeson
--import Network.HTTP.Client


--buildRequest :: String -> RequestBody -> IO Request
--buildRequest url body = do
--  nakedRequest <- parseRequest url
--  return (nakedRequest { method = "POST", requestBody = body })

--send :: RequestBody -> IO ()
--send s = do
--  manager <- newManager defaultManagerSettings
--  request <- buildRequest "http://httpbin.org/post" s
--  response <- httpLbs request manager
--  let Just obj = decode (responseBody response)
--  print (obj :: Object)


data Foo = Foo 
   { 
   _bar :: [Int]
   , _baz :: Int
   }
   deriving (Show, Eq, Ord)
makeLenses ''Foo


data Code = Code {
    _vals :: M.Map String Foo
	}
   deriving (Show, Eq, Ord)
makeLenses ''Code


foofoo :: Foo -> [Int] -> [Int]
foofoo a b = (_bar ( (over (bar . mapped) (+2) a))) ++ b




--fool :: Foo -> Int -> Int
--fool a b = foldr (+) b (_bar a) 



moo :: Foo
moo = Foo { _bar = [2,2,3,4,5,6,6], _baz =1}


boo :: Code
boo = Code {_vals = M.fromList [("A", moo)] }

makeFoo :: Int -> Foo
makeFoo x = Foo {_bar= [1,2,3], _baz = x  }


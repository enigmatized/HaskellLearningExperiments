{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fwarn-missing-methods #-}

module Main (main) where

import Control.Exception.Lens
import Lib


import Control.Lens
--import Control.Monad.Error
--import Control.Monad.Error.Lens

-- Here is a fairly typical situation, where we have low level errors in certain
-- systems, a top level application error type that unifies them

data TopLevel = TopLevelN NetworkBad | TopLevelD DiskBad deriving Show

data NetworkBad = SocketBad FilePath | TimeoutBad Int deriving Show
data DiskBad = FileBad FilePath deriving Show

-- Make classy prisms gives us a type class encapsulating whether data can
-- be made into any of the error types

makeClassyPrisms ''NetworkBad
makeClassyPrisms ''DiskBad
makeClassyPrisms ''TopLevel

main :: IO ()
main =
   catching _AssertionFailed (assert False (return "uncaught")) $ \ _ -> return "caught" 
   someFunc

module Main where

import Data.List
import System.Environment
import QueryBuilder

main = getArgs >>=
  \args -> (putStrLn "running with:") >>
           (putStrLn (show args)) >>
           (buildQuery (Data.List.head args) (Data.List.head (Data.List.tail args))) >>
           return ()

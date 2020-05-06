module Main where

import Data.List
import System.Environment
import System.Exit
import Parser

-- main :: IO ()
main = getArgs >>=
               \args -> (putStrLn "running with:") >>
                        (putStrLn (show args)) >>
                        (runParser (Data.List.head args)) >>
                        return ()


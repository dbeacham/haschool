module Main where

import System.Environment

main :: IO ()
main = do
  (a:b:_) <- getArgs
  let aPlusB = (read a :: Int) + read b
  putStrLn $ a ++ " + " ++ b ++ " = " ++ (show aPlusB)

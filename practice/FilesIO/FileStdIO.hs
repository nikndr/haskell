module Main where

import Counting

main :: IO()
main = do
  putStr "File> "
  nf <- getLine
  file <- readFile nf
  putStr $ counting file

main = do
  ns <- getArgs
  fc <- if null ns then getContents else readAll ns
  putStrLn $ counting fc

readAll :: [String] -> IO String
readAll [] = return ""
readAll (n:ns) = do
  cf1 <- readFile n
  cfa <- readAll ns
  return (cf1 ++ "\n" ++ cfa)

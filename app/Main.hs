module Main where

import           Parser
import           Solver
import           System.Exit     (exitSuccess)
import           Text.Megaparsec (runParser)

main = do
  putStrLn "Please enter a logical formula, :q to quit"
  s <- getLine
  case s of
    ":q" -> exitSuccess
    _ -> do
      case runParser pFormula "stdin" s of
        Right r -> do
          putStrLn $ unlines [
              "You entered: ",
              show r,
              "Solutions are: ",
              showSolutions (solve r)
            ]
        Left err ->
          putStrLn $ "Failed to parse: " ++ show err
      main

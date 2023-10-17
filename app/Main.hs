module Main where

import           Parser
import           Solver
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec
import           Types

repl :: IO ()
repl = do
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
          putStrLn $ unlines ["Failed to parse: ", show err]
      repl

main = do
  args <- getArgs
  case args of
    (flag : fname : _) | flag == "-f" -> do
      handle <- openFile fname ReadMode
      contents <- hGetContents handle
      let ps = [ p | (Right p) <- runParser pFormula "file" <$> lines contents]
      let p = foldl1 And ps

      putStrLn $ unlines [
          "You entered: ",
          show p,
          "Solutions are: ",
          showSolutions (solve p)
        ]

    _ -> repl

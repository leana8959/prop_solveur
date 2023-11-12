module Main where

import Parser
import Solver
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import Types

repl = do
  putStrLn "Please enter a logical formula, :q to quit"
  line <- getLine
  case line of
    ":q" -> exitSuccess
    _ -> do
      case runParser pFormula "stdin" line of
        Right res -> do
          putStrLn
            $ unlines
              [ "You entered: "
              , line
              , "Parsed: "
              , show res
              , "Solutions are: "
              , showSolutions (solve res)
              ]
        Left err ->
          putStrLn $ unlines ["Failed to parse: ", show err]
      repl

fileMode fname =
  do
    handle <- openFile fname ReadMode
    content <- hGetContents handle
    case runParser pFormula "file" content of
      Right res ->
        do
          putStrLn
            $ unlines
              [ "File contains: "
              , show (lines content)
              , "Parsed: "
              , show res
              , "Solutions are: "
              , showSolutions (solve res)
              ]
          exitSuccess
      Left err ->
        do
          putStrLn $ unlines ["Failed to parse: ", show err]
          exitFailure

main = do
  args <- getArgs
  case args of
    (flag : fname : _) | flag == "-f" -> fileMode fname
    _ -> repl

module Main where

import           System.Environment
import           System.Exit
import           System.IO

import           System.Console.ANSI
import           Text.Pretty.Simple  (pPrint)

import           Parser
import           Solver
import           Text.Megaparsec     (errorBundlePretty, runParser)
import           Types

accentStyle = setSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
decorStyle = setSGR [SetColor Foreground Dull Black, SetConsoleIntensity NormalIntensity]
errorStyle = setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
resetStyle = setSGR [Reset]

putInfo prompt text =
  do
    let w = ((80 - length prompt) `div` 2) - 2
    let line = replicate w '='

    decorStyle <* putStr (line ++ " ") <* resetStyle
    accentStyle <* putStr prompt <* resetStyle
    decorStyle <* putStr (" " ++ line) <* resetStyle <* putStrLn ""

    putStrLn text

putError prompt text =
  do
    let w = ((80 - length prompt) `div` 2) - 2
    let line = replicate w '='

    decorStyle <* putStr (line ++ " ") <* resetStyle
    errorStyle <* putStr prompt <* resetStyle
    decorStyle <* putStr (" " ++ line) <* resetStyle <* putStrLn ""

    putStrLn text

repl = do
  putStrLn "Please enter a logical formula, :q to quit"
  line <- getLine
  case line of
    ":q" -> exitSuccess
    _ -> do
      case runParser pFormula "stdin" line of
        Right res -> do
          let sol = solve res
          putInfo "You entered" line
          putInfo "Parsed" "" <* pPrint res
          putInfo "Solutions" (showSolutions sol)
          putInfo ("There are " ++ show (length sol) ++ " solution(s)") ""
        Left err -> putError "Failed to parse" (errorBundlePretty err)
      repl

fileMode fname =
  do
    handle <- openFile fname ReadMode
    content <- hGetContents handle
    case runParser pFormula "file" content of
      Right res ->
        do
          let sol = solve res
          putInfo "File contains" content
          putInfo "Parsed" "" <* pPrint res
          putInfo "Solutions" (showSolutions sol)
          putInfo ("There are " ++ show (length sol) ++ " solution(s)") ""
          exitSuccess
      Left err -> putError "Failed to parse" (errorBundlePretty err)

main = do
  args <- getArgs
  case args of
    (flag : fname : _) | flag == "-f" -> fileMode fname
    _                                 -> repl

module Main (main) where

import System.Environment
import System.Exit
import System.IO

import System.Console.ANSI
import Text.Pretty.Simple (pPrint)

import Parser
import Solver
import Text.Megaparsec (errorBundlePretty, runParser)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

accentStyle, decorStyle, errorStyle, resetStyle :: [SGR]
accentStyle = [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
decorStyle  = [SetColor Foreground Dull Black, SetConsoleIntensity NormalIntensity]
errorStyle  = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
resetStyle  = [Reset]

putWithStyle :: [SGR] -> IO () -> IO ()
putWithStyle st io = setSGR st <* io <* setSGR resetStyle

putWithBorder :: [SGR] -> String -> String -> IO ()
putWithBorder st prompt text = do
  let w = ((80 - length prompt) `div` 2) - 2
      line = replicate w '='
  putWithStyle decorStyle $ putStr (line ++ " ")
  putWithStyle st $ putStr prompt
  putWithStyle decorStyle $ putStrLn (" " ++ line)
  putStrLn text

say, scream :: String -> String -> IO ()
say = putWithBorder accentStyle
scream = putWithBorder errorStyle

doRepl :: IO ()
doRepl = do
  putStrLn "Please enter a logical formula, :q to quit"
  line <- TIO.getLine
  case line of
    ":q" -> exitSuccess
    _ -> do
      case runParser pFormula "stdin" line of
        Right res -> do
          let sol = solve res
          say "You entered" (T.unpack line)
          say "Parsed" "" <* pPrint res
          say "Solutions" (showSolutions sol)
          say ("There are " ++ show (length sol) ++ " solution(s)") ""
        Left err -> scream "Failed to parse" (errorBundlePretty err)
      doRepl

doFile :: FilePath -> IO ()
doFile fname =
  do
    handle <- openFile fname ReadMode
    content <- TIO.hGetContents handle
    case runParser pFormula "file" content of
      Right res ->
        do
          let sol = solve res
          say "File contains" (T.unpack content)
          say "Parsed" "" <* pPrint res
          say "Solutions" (showSolutions sol)
          say ("There are " ++ show (length sol) ++ " solution(s)") ""
          exitSuccess
      Left err -> scream "Failed to parse" (errorBundlePretty err)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (flag : fname : _) | flag == "-f" -> doFile fname
    _                                 -> doRepl

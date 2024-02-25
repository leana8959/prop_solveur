module Main (main) where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI
  (Color(Black, Blue, Red), ColorIntensity(Dull, Vivid),
  ConsoleIntensity(BoldIntensity, NormalIntensity), ConsoleLayer(Foreground),
  SGR(Reset, SetColor, SetConsoleIntensity), setSGR)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)
import Text.Pretty.Simple (pPrint)

import Parser (pFormula)
import Solver (showSolutions, solve)
import Text.Megaparsec (errorBundlePretty, runParser)

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

main :: IO ()
main = do
  args <- getArgs
  (content, isRepl) <- case args of
    (flag : _) | flag == "-" ->
      (, False) <$> TIO.getContents
    (flag : fname : _)
      | flag == "-f" -> do
        handle <- openFile fname ReadMode
        (, False) <$> TIO.hGetContents handle
    _ -> do
      putStrLn "Please enter a logical formula, :q to quit"
      (, True) <$> TIO.getLine

  say "File contains" (T.unpack content)

  let output = bimap errorBundlePretty solve
             . runParser pFormula (if isRepl then "repl" else "file")
             $ content

  case output of
    Right ast -> do
      say "Parsed" "" <* pPrint ast
      say "Solutions" (showSolutions ast)
      say ("There are " ++ show (length ast) ++ " solution(s)") ""
    Left err -> scream "Failed to parse" err

  when isRepl main

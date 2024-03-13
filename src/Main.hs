{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
  (ParserInfo, execParser, flag', fullDesc, header, help, helper, info, long,
  progDesc, short, strOption, (<**>), (<|>))
import System.Console.ANSI
  (Color(Black, Blue, Red), ColorIntensity(Dull, Vivid),
  ConsoleIntensity(BoldIntensity, NormalIntensity), ConsoleLayer(Foreground),
  SGR(Reset, SetColor, SetConsoleIntensity), setSGR)
import System.IO (IOMode(ReadMode), openFile)
import Text.Pretty.Simple (pPrint)

import Parser (pFormula)
import Solver (showSolutions, solve)
import System.Exit (exitSuccess)
import Text.Megaparsec (errorBundlePretty, runParser)

accentStyle, decorStyle, errorStyle, resetStyle :: [SGR]
accentStyle = [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
decorStyle  = [SetColor Foreground Dull Black, SetConsoleIntensity NormalIntensity]
errorStyle  = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
resetStyle  = [Reset]

putWithStyle :: [SGR] -> IO () -> IO ()
putWithStyle st io = setSGR st <* io <* setSGR resetStyle

putWithBorder :: [SGR] -> Text -> Text -> IO ()
putWithBorder st prompt text = do
  let w    = ((80 - T.length prompt) `div` 2) - 2
      line = replicate w '='
  putWithStyle decorStyle $ putStr (line ++ " ")
  putWithStyle st $ TIO.putStr prompt
  putWithStyle decorStyle $ putStrLn (" " ++ line)
  TIO.putStrLn text

say, scream :: Text -> Text -> IO ()
say = putWithBorder accentStyle
scream = putWithBorder errorStyle

data Mode = Repl | File String | Stdin

argsParser :: ParserInfo Mode
argsParser = withInfo (p <**> helper)
  where
    withInfo = flip info
      ( fullDesc
        <> header "prop_solveur - a toy logic solver"
        <> progDesc "Solve logic formulaes"
      )
    p = flag' Repl ( long "repl"
                     <> short 'r'
                     <> help "Solve interactively"
                   )
      <|> do
            fname <- strOption (long "file" <> short 'f' <> help "Read from file, use dash to mean stdin")
            return $ case fname of
              "-" -> Stdin
              _   -> File fname

main :: IO ()
main = do
  mode <- execParser argsParser

  (content, isRepl) <- case mode of
    File fname -> do
      handle <- openFile fname ReadMode
      (, False) <$> TIO.hGetContents handle

    Stdin -> do
      (, False) <$> TIO.getContents

    Repl -> do
      putStrLn "Please enter a logical formula, :q to quit"
      line <- TIO.getLine
      if line == ":q"
        then exitSuccess
        else return (line, True)

  say "File contains" content

  let output = first errorBundlePretty
             . runParser pFormula (if isRepl then "repl" else "file")
             $ content

  case output of
    Right ast -> do
      let sols = solve ast
      say "Parsed" "" <* pPrint ast
      say "Solutions" (T.pack $ showSolutions sols)
      say ("There are " <> (T.pack . show $ length sols) <> " solution(s)") ""
    Left err -> scream "Failed to parse" (T.pack err)

  when isRepl main

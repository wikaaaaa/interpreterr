module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn, putStr
  , FilePath
  , getContents, readFile
  )

import System.IO (hPutStrLn, stderr)
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when, void )

import AbsGramar   ()
import LexGramar   ( Token, mkPosToken )
import ParGramar   ( pProgram, myLexer )
import PrintGramar ( Print, printTree )
import SkelGramar  ()

import Interpreter.Interpreter
import TypeChecker.TypeChecker

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

--runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p

--run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn err
      exitFailure
    Right tree -> do
      case runTypeChecker tree of
        Left error -> hPutStrLn stderr $ "TypeChecker error: " ++ error
        Right _ -> do
          interpreter_res <- runInterpreter tree
          case interpreter_res of
            Left e -> hPutStrLn stderr $ "runtime error: " ++ e
            Right f -> putStr("")
          
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Interpret stdin."
    , "  (file)         Interpret content of file."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pProgram
    fs         -> mapM_ (runFile 2 pProgram) fs


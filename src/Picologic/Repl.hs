module Picologic.Repl (
  repl,
  stdin,
  file,
  runRepl,
) where

import Picologic.AST
import Picologic.Parser
import Picologic.Solver
import Picologic.Pretty

import Text.PrettyPrint

import Control.Monad.State

import Data.List (isPrefixOf)
import System.Console.Haskeline

-------------------------------------------------------------------------------
-- REPL
-------------------------------------------------------------------------------

type Repl = StateT IState (InputT IO)

data IState = IState
  { _module :: Maybe Expr
  , _curFile :: Maybe FilePath
  }

initState :: IState
initState = IState Nothing Nothing

preamble :: Repl ()
preamble = liftIO $ do
  putStrLn "Picologic 0.1"
  putStrLn "Type :help for help"

runRepl :: Repl a -> IO IState
runRepl f = runInputT defaultSettings (execStateT (preamble >> f) initState)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

load :: String -> Repl ()
load arg = do
  liftIO $ putStrLn $ "Loading " ++ arg
  file arg
  modify $ \s -> s { _curFile = Just arg }

reload :: Repl ()
reload = do
  fname <- gets _curFile
  case fname of
    Just fname' -> handleCommand "load" [fname']
    Nothing -> liftIO $ putStrLn "No such file"

run :: Repl ()
run = do
  mod <- gets _module
  case mod of
    Nothing -> liftIO $ putStrLn "No expression."
    Just ex -> liftIO $ do
      sols <- solveProp ex
      putStrLn "Solutions:"
      putStrLn (ppSolutions sols)

clauses :: Repl ()
clauses = do
  env <- gets _module
  case env of
    Just ex -> liftIO $ print (clausesExpr ex)
    Nothing -> liftIO $ putStrLn "No file loaded."

help :: Repl ()
help = liftIO $ do
  putStrLn ":clauses      Show the SAT solver clauses"
  putStrLn ":load <file>  Load a program from file"

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

-- evaluate a module
modl :: FilePath -> Repl ()
modl fname = do
  mod <- liftIO $ parseFile fname
  case mod of
    Left  err -> liftIO $ print err
    Right res ->
      let ex = (simp . cnf $ res) in
      modify $ \s -> s { _module = Just ex }

-- evaluate an expression
exec :: String -> Repl ()
exec source = do
  let mod = parseExpr source
  case mod of
    Left  err -> liftIO $ print err
    Right res -> do
      let ex = (simp . cnf $ res)
      liftIO $ putStrLn (render $ ppExprU ex)
      liftIO $ do
        sols <- solveProp ex
        putStrLn "Solutions:"
        putStrLn (ppSolutions sols)
      modify $ \s -> s { _module = Just ex }

file :: FilePath -> Repl ()
file fname = modl fname

stdin :: IO ()
stdin = do
  contents <- getContents
  case parseExpr contents of
    Left err -> print err
    Right ex -> do
      sols <- solveProp ex
      putStrLn (ppSolutions sols)

handleCommand :: String -> [String] -> Repl ()
handleCommand cmd args
    | "ru"  `isPrefixOf` cmd = run
    | "r"   `isPrefixOf` cmd = reload
    | "cl"  `isPrefixOf` cmd = clauses
    | "h"   `isPrefixOf` cmd = help
    | length args == 0 = liftIO $ putStrLn "Not enough arguments"
    | "l"  `isPrefixOf` cmd = load arg
    | otherwise = liftIO $ putStrLn "Unknown command"
  where
    arg = head args

repl :: Repl ()
repl = do
  minput <- lift $ getInputLine "Logic> "
  case minput of
    Nothing -> lift $ outputStrLn "Goodbye."

    Just (':' : cmds) -> do
      let (cmd:args) = words cmds
      handleCommand cmd args
      repl

    Just input -> do
      exec input
      repl

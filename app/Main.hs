module Main where

import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import Data.List ((\\), union)
import qualified Data.Map as Map
import Control.Monad.State
import Parse
import LangLib
import AST

type Variables = Map.Map Name Term
type LState = StateT Variables IO

substitute :: Name -> Term -> Term -> Term
substitute replName withTerm term = sub term
  where
    sub (Var name)
      | name == replName = withTerm
    sub (Lam name term)
      | name /= replName = Lam name (sub term)
    sub (App term term') = App (sub term) (sub term')
    sub term = term

alphaConv :: Name -> Name -> Term -> Term
alphaConv replName withName = substitute replName (Var withName)

betaRedux :: Term -> Term
betaRedux (App (Lam replName term) withTerm) = substitute replName withTerm term
betaRedux (App term term') = App (betaRedux term) (betaRedux term')
betaRedux (Lam name term) = Lam name (betaRedux term)
betaRedux term = term

reduce :: Term -> Term
reduce term
  | term' == term = term
  | otherwise = reduce term'
  where
    term' = betaRedux term

reduce' :: Term -> LState Term
reduce' term
  | term' == term = return term
  | otherwise = do
    liftIO $ print term
    reduce' term'
  where
    term' = betaRedux term

fv :: Term -> [Name]
fv (Var name) = [name]
fv (Lam name term) = fv term \\ [name]
fv (App term term') = fv term `union` fv term'

replVars :: Term -> Variables -> Term
replVars term vars = foldr (\var term' -> if Map.member var vars then substitute var (vars Map.! var) term' else term') term (fv term)

eval :: Cmd -> LState ()

eval Quit = do
  liftIO $ putStrLn "Leaving."

eval (Eval term) = do
  term <- gets (reduce . replVars (convert term))
  liftIO $ print term
  loop

eval (Assign name term) = do
  term <- gets (reduce . replVars (convert term))
  modify $ Map.insert name term
  loop

eval (Alpha term replName withName) = do
  term <- gets (alphaConv replName withName . reduce . replVars (convert term))
  liftIO $ print term
  loop
  
eval (Beta term) = do
  term <- get >>= (reduce' . replVars (convert term))
  liftIO $ print term
  loop

eval (FV term) = do
  vars <- gets (fv . reduce . replVars (convert term))
  liftIO $ print vars
  loop

loop :: LState ()
loop = do
  liftIO $ putStr "lam> "
  liftIO $ hFlush stdout
  inp <- liftIO getLine
  case parse parseCmd "stdin" inp of
    Left error -> do
      liftIO . putStrLn $ "Trouble parsing:\n" ++ show error
      loop
    Right cmd -> do
      eval cmd

main :: IO ()
main = do
  putStrLn "lam v0.01"
  execStateT loop Map.empty
  return ()

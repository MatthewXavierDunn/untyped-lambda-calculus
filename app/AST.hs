module AST where

type Name = String

data Cmd = Quit
         | Eval InTerm
         | Assign Name InTerm
         | Alpha InTerm Name Name
         | Beta InTerm
         | FV InTerm
         deriving (Show)


{-- Intermediate Language --}


data InTerm = InVar Name
            | InLam [Name] InTerm
            | InApp [InTerm]
            | InOp String InTerm InTerm
            | InLet [Name] InTerm InTerm
            | InLetrec [Name] InTerm InTerm
            | InIfThenElse InTerm InTerm InTerm
            | InLit Lit
            | InParen InTerm
            deriving (Show)

data Lit = LitNat Int
         | LitBool Bool
         | LitList [InTerm]
         deriving (Show)


{-- Lambda Calculus --}

data Term = Var Name
          | Lam Name Term
          | App Term Term
          deriving (Eq)

instance Show Term where
  show (Var name) = name
  show (Lam name term) = "(\\" ++ unwords vars ++ " . " ++ show body ++ ")"
    where
      (vars, body) = flatten term [name]
      flatten (Lam name term) names = flatten term (names ++ [name])
      flatten term names = (names, term)
  show (App term term') = "(" ++ unwords (map show terms) ++ ")"
    where
      terms = flatten term [term']
      flatten (App term term') terms = flatten term (term' : terms)
      flatten term terms = term : terms

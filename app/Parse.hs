module Parse where

import Text.ParserCombinators.Parsec hiding (token)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.Functor.Identity (Identity)
import AST


{-- Small Parsers --}

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef {
  T.identStart = letter <|> char '_',
  T.identLetter = alphaNum <|> char '_',
  T.reservedNames = [ "let", "letrec", "in", "True", "False", "if", "then", "else"],
  T.reservedOpNames = [ "=", "\\", "."]
}

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser languageDef

identifier :: Parser Name
identifier = T.identifier lexer

token :: Parser a -> Parser a
token p = do
  x <- p
  spaces
  return x

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

operator :: Parser String
operator = T.operator lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

symbol :: String -> Parser String
symbol s = token $ string s

natural :: Parser Int
natural = fromIntegral <$> T.natural lexer


{-- Big Parsers --}

parseCmd :: Parser Cmd
parseCmd = do
  spaces
  parseSpecCmd <|> (Eval <$> try parseExpr)

parseSpecCmd :: Parser Cmd
parseSpecCmd = do
  char ':'
  parseQuit <|> parseAlpha <|> parseBeta <|> parseFV

parseQuit :: Parser Cmd
parseQuit = do
  symbol "q"
  return Quit

parseAlpha :: Parser Cmd
parseAlpha = do
  symbol "a"
  term <- parseExpr
  symbol "<<"
  name <- identifier
  symbol "<<"
  Alpha term name <$> identifier

parseBeta :: Parser Cmd
parseBeta = do
  symbol "b"
  Beta <$> parseExpr

parseFV :: Parser Cmd
parseFV = do
  symbol "f"
  FV <$> parseExpr

parseAssign :: Parser Cmd
parseAssign = do
  name <- identifier
  vars <- many identifier
  reservedOp "="
  Assign name . InLam vars <$> parseExpr

parseExpr :: Parser InTerm
parseExpr = parseLet <|> parseLetrec <|> parseIfThenElse <|> parseTerm

parseLet :: Parser InTerm
parseLet = do
  reserved "let"
  names <- many1 identifier
  reservedOp "="
  term <- parseExpr
  reserved "in"
  InLet names term <$> parseExpr

parseLetrec :: Parser InTerm
parseLetrec = do
  reserved "letrec"
  names <- many1 identifier
  reservedOp "="
  term <- parseExpr
  reserved "in"
  InLetrec names term <$> parseExpr

parseIfThenElse :: Parser InTerm
parseIfThenElse = do
  reserved "if"
  cond <- parseExpr
  reserved "then"
  term <- parseExpr
  reserved "else"
  InIfThenElse cond term <$> parseExpr

parseTerm :: Parser InTerm
parseTerm = chainl1 (InApp <$> token (many1 term)) (InOp <$> operator)
  where
    term :: Parser InTerm
    term = (InParen <$> parens parseTerm) <|> parseLambda <|> parseVariable <|> (InLit <$> parseLit)

parseLambda :: Parser InTerm
parseLambda = do
  reservedOp "\\"
  vars <- many1 identifier
  reservedOp "."
  InLam vars <$> parseTerm

parseVariable :: Parser InTerm
parseVariable = InVar <$> identifier

parseLit :: Parser Lit
parseLit = parseNatural <|> parseBool <|> parseList

parseNatural :: Parser Lit
parseNatural = LitNat <$> natural

parseBool :: Parser Lit
parseBool = true <|> false
  where
    true = do
      reserved "True"
      return (LitBool True)
    false = do
      reserved "False"
      return (LitBool False)

parseList :: Parser Lit
parseList = LitList <$> brackets (commaSep parseTerm)

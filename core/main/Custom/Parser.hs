module Custom.Parser(parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex

import Lexer
import ParserSupport (contents)
import Custom.Syntax

number :: Parser Expr
number = do
  n <- integer
  return $ Constant n

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops = [[binary "*" Ex.AssocLeft,
           binary "/" Ex.AssocLeft,
           binary "√" Ex.AssocLeft]
         ,[binary "+" Ex.AssocLeft,
           binary "-" Ex.AssocLeft]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor =  try number
      <|> try call
      <|> try variable
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def
    
parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

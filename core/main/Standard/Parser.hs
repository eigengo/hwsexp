module Standard.Parser(parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Char as Ch

import Parser (range, contents)
import Lexer
import Standard.Syntax

toplevel :: Parser Expr
toplevel = do
  Ch.string "evendistr"
  whitespace
  count  <- range
  whitespace
  values <- range
  whitespace
  return $ EvenDistr count values

parseToplevel :: String -> Either ParseError Expr
parseToplevel s = parse (contents toplevel) "<stdin>" s

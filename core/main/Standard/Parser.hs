module Standard.Parser(parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Char as Ch

import Parser (range)
import Standard.Syntax

toplevel :: Parser Expr
toplevel = do
  Ch.string "evendistr"
  count  <- range
  values <- range
  return $ EvenDistr count values

parseToplevel :: String -> Either ParseError Expr
parseToplevel s = parse toplevel "<stdin>" s


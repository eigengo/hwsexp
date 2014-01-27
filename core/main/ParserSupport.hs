module ParserSupport(range, contents) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Char as Ch

import Lexer
import Syntax

-- |Returns a parser that skips initial whitespace and then parses
--  the entire input, expecting @<EOF>@ when it is done parsing
contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r

-- |Range is "[" @decimal ".." @decimal@ "]" or @decimal@, for example
--  5 ~> Exact 5
--  [0..3] ~> Between 0 3
--  [1..0] ~> fail 
range :: Parser Range
range = 
  between <|> exact <?> "Range"
  where
  between = do
    Ch.char '['
    lower <- int
    Ch.string ".."
    upper <- int
    Ch.char ']'
    if (lower > upper) then fail "Range: lower > upper!" else return $ Between lower upper
  exact = do
    val <- int
    return $ Exact val

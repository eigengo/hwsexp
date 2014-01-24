module Parser(parseExpression, range, contents) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Char as Ch

import Control.Applicative ((<$>))

import Lexer
import Syntax

{--
do { ; ; ; } forever every ...
{ ... } forever every ...
--}

parseExpression :: String -> Either ParseError Expression
parseExpression = parse (contents expression) "<stdin>"

block :: Parser String
block = do
  Ch.string "{"
  manyTill anyChar (try (Ch.string "}"))

expression :: Parser Expression
expression = do
  dis <- custom <|> standard
  whitespace
  rep <- option defaultRepetition repetition
  whitespace
  del <- option defaultDelay delay
  return $ Expression dis rep del
  where 
    defaultRepetition = Forever
    defaultDelay = Fixed (Exact 1000000)
    custom = do
      Ch.string "do"
      whitespace
      Custom <$> block
    standard = Standard <$> block


contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r
    
-- |Parses the repetition statement; which is either
--  * @forever@
--  * @range@ "times"
--  * "once"
repetition :: Parser Repetition
repetition = do
  forever <|> times <|> once <?> "Repetition"
  where
    forever = do
      Ch.string "forever" 
      return Forever
    once = do
      Ch.string "once"
      return $ Times (Exact 1)
    times = do
      rep <- range
      whitespace
      Ch.string "times"
      return $ Times rep

-- |Parses the delay statement; at the moment, we only have fixed delay with
--  with "every" @range@
delay :: Parser Delay
delay = do 
  Ch.string "every"
  val <- range
  Ch.string "ms"
  return $ Fixed (mult val 1000)
  <?> "Delay"
  where
    mult :: Range -> Int -> Range
    mult (Exact x)     y = Exact   (x * y)
    mult (Between l u) y = Between (l * y) (u * y)

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

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

{--
do { ; ; ; }  forever every ...
evendistr ... forever every ...

--}


expression :: Parser Expression
expression = do 
  dis <- distribution
  rep <- option defaultRepetition repetition
  del <- option defaultDelay delay
  C.endOfInput
  return $ Expression dis rep del
  where 
    defaultRepetition = Forever
    defaultDelay = Fixed (Exact 1000000)

-- |Distribution parses the thing to generate. ATM, we only have @evendistr count values@,
--  where count is the number of repetitions and values are the numbers to generate
distribution :: Parser Distribution
distribution = do
  choice [even] <?> "Distribution"
  where
    even = do
      C.string "evendistr" 
      C.skipSpace 
      count <- range
      C.skipSpace
      rng <- range
      return $ EvenDistr count rng
    
-- |Parses the repetition statement; which is either
--  * @forever@
--  * @range@ "times"
--  * "once"
repetition :: Parser Repetition
repetition = do
  choice [forever, times, once] <?> "Repetition"
  where
    forever = do
      C.string "forever" 
      C.skipSpace
      return Forever
    once = do
      C.string "once"
      C.skipSpace
      return $ Times (Exact 1)
    times = do
      rep <- range
      C.skipSpace
      C.string "times"
      C.skipSpace
      return $ Times rep

-- |Parses the delay statement; at the moment, we only have fixed delay with
--  with "every" @range@
delay :: Parser Delay
delay = do 
  C.string "every"
  C.skipSpace
  val <- range
  C.string "ms"
  C.skipSpace
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
  choice [between, exact] <?> "Range"
  where
  between = do
    C.char '['
    lower <- C.decimal 
    C.string ".."
    upper <- C.decimal
    C.char ']'
    C.skipSpace
    if (lower > upper) then fail "Range: lower > upper!" else return $ Between lower (upper + 1)
  exact = do
    val <- C.decimal
    C.skipSpace
    return $ Exact val

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

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
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

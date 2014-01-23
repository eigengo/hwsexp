module Standard.Parser(parseToplevel) where

import Text.Parsec

--import qualified Text.Parsec.Expr as Ex
--import qualified Text.Parsec.Token as Tok

--import Lexer
import Custom.Syntax

parseToplevel :: String -> Either ParseError Expr
parseToplevel = fail "Bantha poodoo"
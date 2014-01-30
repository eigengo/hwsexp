module Custom.Syntax(Name, Expr(..)) where

type Name = String

data Expr
  = Constant Integer
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

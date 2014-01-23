module Syntax where

-- |Range is either a single value or between some upper and lower limit
data Range = 
    -- |Exact value
    Exact Int
    -- |Value between lower and upper limit; lower < upper.
  | Between Int Int deriving (Show)

-- |Describes the different values we can generate
data Distribution = 
    -- |Even distribution of a number of values 
    Standard String 
    -- |Custom generator of values
  | Custom String deriving (Show)

-- |The repetition rule
data Repetition = 
    -- |We repeat the generating step forever
    Forever 
    -- |We repeat specified number of times
  | Times Range deriving (Show)

-- |The delay between steps
data Delay =
  -- |We wait a range of milliseconds
  Fixed Range deriving (Show)

-- |Generator expression combines the thing to generate, number of repetitions and
--  the delay between repetitions
data Expression = Expression Distribution Repetition Delay deriving (Show)

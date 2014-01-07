{-# LANGUAGE OverloadedStrings #-}
module Generator(generator, Generator(..), GeneratorDelay) where

  import System.Random
  import Control.Monad
  import Control.Monad.State
  import Data.Attoparsec 
  import Control.Applicative ((<$>))
  import qualified Data.Attoparsec.Char8 as C
  import qualified Data.ByteString.Char8 as B

  -- |Function that delays the generator for the given number of microseconds.
  --  A suitable value is @threadDelay@
  type GeneratorDelay = Int -> IO ()

  -- |This will need thinking...
  type GeneratorResult a = IO a

  -- |Generator is a type that performs a step of the generation process
  newtype Generator a = Generator { 

    -- |Performs the generator step
    runGenerator :: GeneratorDelay     -- ^The delaying function. You can pass in @threadDelay@, for example.
                 -> GeneratorResult a  -- ^The step result
  }

  -- evendistr 10 [0..100] 
  -- evendistr 10 [0..100] 5 times
  -- evendistr 10 [0..100] [0..5] times
  -- evendistr 10 [0..100] forever 
  -- evendistr 10 [0..100] ... every 1000ms
  -- evendistr 10 [0..100] ... every [1000..2000]ms

  -- |Prepares a generator by parsing the input string. It returns a value that
  --  you can use to get the generated values.
  --   
  --  Typically, you'd have code similar to
  --  @
  --      case (runGenerator threadDelay) <$> (generator "evendistr 5 [1..2] every 50ms") of
  --          Left err  -> ...         -- what do you want, a cookie?
  --          Right gen -> gen >>= ... -- good user!
  -- @
  -- In GHCi, you can try out
  -- @
  --     let (Right nums) = (runGenerator) <$> generator ("evendistr 5 [1..2] every 50ms")
  -- @
  generator :: String                           -- ^The expression to parse
            -> Either String (Generator [Int])  -- ^The result with errors or the ready Generator
  generator input = do
    expr <- parseOnly expression (B.pack input)
    return $ runGenerator expr
    where
      runGenerator :: Expression -> Generator [Int]
      -- TODO: complete the Times repetition
      runGenerator (Expression (EvenDistr countRange valueRange) _ (Fixed delayRange)) = Generator { 
        runGenerator = \wait -> do
          count <- fromRange countRange
          delay <- fromRange delayRange
          wait delay
          replicateM count (fromRange valueRange)
      }
      fromRange :: Range -> IO Int
      fromRange (Exact x) = return x
      fromRange (Between l u) = ((l +) . (`mod` (u - l)) <$> (randomIO :: IO Int))

  -- |Range is either a single value or between some upper and lower limit
  data Range = 
      -- |Exact value
      Exact Int 
      -- |Value between lower and upper limit; lower < upper.
    | Between Int Int deriving (Show)

  -- |Describes the different values we can generate
  data Generate = 
    -- |Even distribution of a number of values 
    EvenDistr Range Range deriving (Show)

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
  data Expression = Expression Generate Repetition Delay deriving (Show)

  expression :: Parser Expression
  expression = do 
    gen <- generate
    rep <- option defaultRepetition repetition
    del <- option defaultDelay delay
    C.endOfInput
    return $ Expression gen rep del
    where 
      defaultRepetition = Forever
      defaultDelay = Fixed (Exact 1000000)

  -- |Generate parses the thing to generate. ATM, we only have @evendistr count values@,
  --  where count is the number of repetitions and values are the numbers to generate
  generate :: Parser Generate
  generate = do
    C.string "evendistr" 
    C.skipSpace 
    count <- range
    C.skipSpace
    rng <- range
    return $ EvenDistr count rng
    <?> "Generate"

  -- |Parses the repetition statement; which is either
  --  * @forever@
  --  * @range@ "times"
  repetition :: Parser Repetition
  repetition = do
    choice [forever, times] <?> "Repetition"
    where
      forever = do
        C.string "forever" 
        C.skipSpace
        return Forever
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
    exact = Exact <$> C.decimal
  
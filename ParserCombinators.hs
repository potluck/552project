{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserCombinators where

import Parser
import Control.Monad
import Data.Char
import System.IO

            
type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors.
-- However, for compatibility with Parsec, we give this function 
-- the same type.
parse :: Parser b a -> [b] -> Either ParseError a
parse parser str = case (doParse parser str) of 
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"
    
parseFromFile :: Parser Char a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do 
  handle <- openFile filename ReadMode 
  str <- hGetContents handle
  return $ parse parser str  
  
  
-- | Parsers for specific sorts of characters 
alpha, digit, upper, lower, space :: Parser Char Char
alpha = satisfy isAlpha
digit = satisfy isDigit            
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace
   
-- | Parsers and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Eq b => b -> Parser b b
char c = satisfy (c ==)

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: Eq b => [b] -> Parser b [b]
string = mapM char

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Char Int
int = do n <- string "-" <|> return []
         s <- many1 digit  
         return $ (read (n ++ s) :: Int)

-- | succeed only if the input is a (positive or negative) number
double :: Parser Char Double
double = do n <- string "-" <|> return []
            s1 <- many1 digit  
            d  <- string "."    
            s2 <- many1 digit     
            return $ (read (n ++ s1 ++ d ++ s2) :: Double)


-- | given a parser, apply it as many times as possible                         
-- and return the answer in a list
many   :: Parser b a -> Parser b [a]
many p = many1 p <|> many0 
   where many0 = return []
                    
-- | given a parser, apply it as many times as possible,
-- but at least once.
many1 :: Parser b a -> Parser b [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: Parser a b -> Parser a (b -> b -> b) -> b -> Parser a b
chainl p op x = chainl1 p op <|> return x

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: Parser b a -> Parser b (a -> a -> a) -> Parser b a
p `chainl1` pop = p >>= rest
    where rest x = next x <|> return x 
          next x = do o <- pop
                      y <- p
                      rest $ x `o` y 
                      
-- | Combine all parsers in the list (sequentially)
choice :: [Parser b a] -> Parser b a
choice = foldr (<|>) (fail "")

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: Parser b open -> Parser b a -> Parser b close -> Parser b a
between open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser b a -> Parser b sep -> Parser b [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser b a -> Parser b sep -> Parser b [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))



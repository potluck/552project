
-- Advanced Programming, HW 4
-- by Pulak Mittal <pulak@>, Thanat Owlarn <towlarn@>

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Parser (Parser,                  
                   get,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,  
                   ) where

import Control.Monad

newtype Parser b a  = P ([b] -> [(a, [b])])

doParse :: Parser b a -> [b] -> [(a, [b])] 
doParse (P p) s = p s

-- | Return the next character
-- (this was called 'oneChar' in lecture)
get :: Parser a a
get = P (\cs -> case cs of 
                (x:xs) -> [ (x,xs) ]
                []     -> [])

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (a -> Bool) -> Parser a a
satisfy p = do c <- get
               if (p c) then return c else fail "End of input"

instance Monad (Parser b) where
   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
                             doParse (fp2 a) cs') 

   return x   = P (\cs -> [ (x, cs) ])

   fail _     = P (\_ ->  [ ] )

instance Functor (Parser b) where
   fmap f p = do x <- p
                 return (f x)

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: Parser a b -> Parser a b -> Parser a b
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: Parser a b -> Parser a b -> Parser a b
p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]

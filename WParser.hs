
import Control.Monad

import Parser
import ParserCombinators
import WhilePP

import Test.HUnit



valueP :: Parser Char Value
valueP = intP <|> boolP <|> nullP <|> doubleP <|> charP <|> varP --listP

intP :: Parser Char Value
intP = do
  x <- int
  return (IntVal x)
  
constP :: Eq b => [b] -> a -> Parser b a
constP s x = do
  z <- string s
  return x

boolP :: Parser Char Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

nullP :: Parser Char Value
nullP = constP "null" Null

doubleP :: Parser Char Value
doubleP = do
  x <- double
  return (DoubleVal x)
  
charP :: Parser Char Value
charP = do
  s1 <- string "'"
  c  <- get
  s2 <- string "'"
  return (CharVal c)
    
{-
listP :: Parser Char Value    
listP = (parenP '[' listP ']')

ilistP :: Parser Char Value
ilistP = chainl' (wsP valueP <|> ilistP) (wsP $ commaP) (List [])

chainl' :: Parser a b -> Parser a b -> b -> Parser a b
chainl' p op x = chainl1' p op <|> return x

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1' :: Parser b a -> Parser b a -> Parser b a
p `chainl1'` pop = p >>= rest
    where rest x = next x <|> return x 
          next x = do o <- pop
                      y <- p
                      rest $ x `o` y 
-}         
                      
               
parenP :: Char -> Parser Char a -> Char -> Parser Char a
parenP l p r = do wsP (char l)
                  x <- wsP p
                  wsP (char r)
                  return x
    
    
opP :: Parser Char Bop 
opP = choice [
  addopP,
  mulopP,
  boolopP
  ]
      
addopP :: Parser Char Bop
addopP = choice [
  constP "+" Plus,
  constP "-" Minus
  ]
         
mulopP :: Parser Char Bop
mulopP = choice [
  constP "*" Times,
  constP "/" Divide
  ]
         
boolopP :: Parser Char Bop
boolopP = choice [
  constP ">=" Ge,
  constP ">" Gt,
  constP "<=" Le,
  constP "<" Lt
  ]

varP :: Parser Char Value
varP = do
  x <- upper
  xs <- many upper
  return (Var $ x:xs)

wsP :: Parser Char a -> Parser Char a
wsP p = do
  x <- p
  many space
  return x
  
{-
commaP :: Parser Char Value
commaP = do
  x <- many $ string ","
  return ","
-}



evalueP :: Parser Char Expression
evalueP = do 
  x <- valueP
  return (Val x)

makeopP :: Parser b Bop -> Parser b (Evalable -> Evalable -> Expression)
makeopP x = do
  y <- x
  return (Op y)

eaddopP :: Parser Char (Evalable -> Evalable -> Expression)
eaddopP = makeopP addopP

emulopP :: Parser Char (Evalable -> Evalable -> Expression)
emulopP = makeopP mulopP

eboolopP :: Parser Char (Evalable -> Evalable -> Expression)
eboolopP = makeopP boolopP  
  
leftExprP :: Parser Char Expression  
leftExprP = (parenP '(' exprP ')') <|> evalueP <|> derefP

exprP :: Parser Char Expression
exprP = (wsP boolP) `echainl1` (wsP eboolopP) where
  boolP = (wsP sumP) `echainl1` (wsP eaddopP)
  sumP = (wsP mulP) `echainl1` (wsP emulopP)
  mulP = (wsP leftExprP)
  
  
-- a = char; b = expression; c = evalable
echainl :: Parser Char Expression -> Parser Char (Evalable -> Evalable -> Expression) -> Expression -> Parser Char Expression
echainl p op x = echainl1 p op <|> return x

-- b = char; c = evalable; a = expression
-- | Like 'chainl', but parses one or more occurrences of @p@.
echainl1 :: Parser Char Expression -> Parser Char (Evalable -> Evalable -> Expression) -> Parser Char Expression
p `echainl1` pop = p >>= rest
    where rest x = next x <|> return x 
          next x = do o <- pop
                      y <- p
                      rest $ (Expr x) `o` (Expr y)

derefP :: Parser Char Expression
derefP = do 
  _ <- wsP $ string "*"
  x <- wsP variableP
  return (Dereference x)
  
variableP :: Parser Char Variable
variableP = do
  x <- upper
  xs <- many upper
  return (x:xs)
  

-- Tests
oneV,twoV,threeV :: Evalable
oneV   = Expr $ Val (IntVal 1)
twoV   = Expr $ Val (IntVal 2)
threeV = Expr $ Val (IntVal 3)

t11 :: Test
t11 = TestList [
  "s1" ~: succeed (parse exprP "1 "),
  "s2" ~: succeed (parse exprP "1  + 2"), 
  "s3" ~: notsucceed (parse exprP "+2"), 
  "s4" ~: notsucceed (parse exprP ""),
  Right e1 ~=? parse exprP "1 + 2 + 3",
  Right e2 ~=? parse exprP "1 - 2 + 3",
  Right e1 ~=? parse exprP "(1+   2) + (3)",
  Right e1 ~=? parse exprP "((1+(2)) +(3)) ",
  Right e3 ~=? parse exprP "3 <= (3 * (2 - 1))",
  Right e3 ~=? parse exprP "3 <= (3 * (2 - 1))",
  Right e4 ~=? parse exprP "((1 + 2) * 3) < (3 * (2 - 1))",
  Right e5 ~=? parse exprP "1 < 2 + 3",
  Right e6 ~=? parse exprP "3 + 2 < 1",
  Right (Op Ge (Expr e3) (Expr e4)) ~=? parse exprP "(3 <=3*(2-1)) >= ((1+2)*3 < 3*(2-1))",
  Right e7 ~=? parse exprP "1<2<3",
  -- The parens are invalid and so parsing stops after the 1+2
  Right (Op Plus oneV twoV) ~=? parse exprP "1+2(+3)",
  "s5" ~: notsucceed (parse exprP "(1+)2+3")
  ]
  where
    succeed (Left _)     = assert False
    succeed (Right _)    = assert True
    notsucceed (Left _)  = assert True
    notsucceed (Right _) = assert False
    e1 = (Op Plus (Expr $ Op Plus oneV twoV) threeV)
    e2 = (Op Plus (Expr $ Op Minus oneV twoV) threeV)
    e3 = (Op Le threeV (Expr $ Op Times threeV (Expr $ Op Minus twoV oneV)))
    e4 = (Op Lt 
           (Expr $ Op Times (Expr $ Op Plus oneV twoV) threeV) 
           (Expr $ Op Times threeV (Expr $ Op Minus twoV oneV)))
    e5 = (Op Lt oneV (Expr $ Op Plus twoV threeV))
    e6 = (Op Lt (Expr $ Op Plus threeV twoV) oneV)
    e7 = (Op Lt (Expr $ Op Lt oneV twoV) threeV)
    

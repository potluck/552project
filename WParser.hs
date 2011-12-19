
module WParser ( 
  parse,
  exprP,
  statementP,
  funcmapP,
  Store,
  FuncStore
  ) where

import Control.Monad

import Parser
import ParserCombinators
import WhilePP

import Test.HUnit

import Data.Map (Map)
import qualified Data.Map as Map

type Store = Map Variable Value
type FuncStore = Map String Function

valueP :: Parser Char Value
-- parse double BEFORE int!
valueP = doubleP <|> intP <|> boolP <|> nullP <|> charP <|> varP <|> listP

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
  many space
  x <- p
--  many space
  return x
  
  
listP :: Parser Char Value
listP = do
  l <- wsP $ between (wsP (string "[")) (wsP $ ilistP exprP) (wsP (string "]"))
  return $ List l
  
ilistP :: Parser Char a -> Parser Char [a]
ilistP p = do
  x <- upto1 p
  case x of
    [] -> return []
    (y:_) -> do
      xs <- many $ commaP $ wsP p
      return (y:xs)
  
upto1 :: Parser Char a -> Parser Char [a]  
upto1 p = get1 p <|> get0 
  where get0 = return []
        get1 p = do
          x <- wsP p
          return [x]

commaP :: Parser Char a -> Parser Char a
commaP p = do
  wsP $ many $ string ","
  x <- p
  return x



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
  
  

echainl :: Parser Char Expression -> Parser Char (Evalable -> Evalable -> Expression) -> Expression -> Parser Char Expression
echainl p op x = echainl1 p op <|> return x

echainl1 :: Parser Char Expression -> Parser Char (Evalable -> Evalable -> Expression) -> Parser Char Expression
p `echainl1` pop = p >>= rest
    where rest x = next x <|> return x 
          next x = do o <- pop
                      y <- p
                      rest $ (Expr x) `o` (Expr y)

-- NOTE: Dereferencing uses the # sign
derefP :: Parser Char Expression
derefP = do 
  _ <- wsP $ string "#"
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

oneDV, twoDV, threeDV :: Evalable
oneDV   = Expr $ Val (DoubleVal 1.0)
twoDV   = Expr $ Val (DoubleVal 2.5)
threeDV = Expr $ Val (DoubleVal 3.0)

varX, varY, varZ :: Evalable
varX = Expr $ Val $ Var "X"
varY = Expr $ Val $ Var "Y"
varZ = Expr $ Val $ Var "Z"

derefX :: Evalable
derefX = Expr $ Dereference "X"

charA :: Evalable
charA = Expr $ Val $ CharVal 'a'

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
    
    
t12 :: Test
t12 = TestList [
  Right (Val $ DoubleVal 2.5) ~=? parse exprP "2.5  ",
  "f" ~: notsucceed (parse exprP "  ' b'"),
  "f" ~: notsucceed (parse exprP "'ab'"),
  Right e1 ~=? parse exprP "#X + 2.5 + 'a'",
  Right e2 ~=? parse exprP "  #X - 2.5 + 'a'",
  Right e1 ~=? parse exprP "(# X+   2.5) + ('a')",
  Right e1 ~=? parse exprP "((#  X +(  2.5)) +('a')) ",
  Right e3 ~=? parse exprP "'a' <= ('a' * (2.5 - #X))",
  Right e3 ~=? parse exprP "'a' <= ('a' * (2.5 - #X))",
  Right e4 ~=? parse exprP "((# X + 2.5) * 'a') < ('a' * (2.5 - #X))",
  Right e5 ~=? parse exprP "# X < 2.5 + 'a'",
  Right e6 ~=? parse exprP "'a' + 2.5 < # X",
  Right (Op Ge (Expr e3) (Expr e4)) ~=? parse exprP "('a' <='a'*(2.5-#X)) >= ((#X+2.5)*'a' < 'a'*(2.5-#X))",
  Right e7 ~=? parse exprP "#X<2.5<'a'"
  ]
  where
    succeed (Left _)     = assert False
    succeed (Right _)    = assert True
    notsucceed (Left _)  = assert True
    notsucceed (Right _) = assert False
    e1 = (Op Plus (Expr $ Op Plus derefX twoDV) charA)
    e2 = (Op Plus (Expr $ Op Minus derefX twoDV) charA)
    e3 = (Op Le charA (Expr $ Op Times charA (Expr $ Op Minus twoDV derefX)))
    e4 = (Op Lt 
           (Expr $ Op Times (Expr $ Op Plus derefX twoDV) charA) 
           (Expr $ Op Times charA (Expr $ Op Minus twoDV derefX)))
    e5 = (Op Lt derefX (Expr $ Op Plus twoDV charA))
    e6 = (Op Lt (Expr $ Op Plus charA twoDV) derefX)
    e7 = (Op Lt (Expr $ Op Lt derefX twoDV) charA)
    
    


statementP :: Parser Char Statement
statementP = choice [ sequenceP, ifP, whileP, skipP, assignP, assignFP, assignRP, assignFRP, printP, throwP, tryP, returnP, callP]

leftSequenceP :: Parser Char Statement  
leftSequenceP = choice [ ifP, whileP, skipP, assignP, assignFP, assignRP, assignFRP, printP, throwP, tryP, returnP, callP ]

-- helper parsers
assignP :: Parser Char Statement
assignP = do 
  x <- wsP varP
  wsP (string "=")
  z <- wsP exprP
  return (Assign (Val x) z)
  
assignFP :: Parser Char Statement
assignFP = do 
  x <- wsP varP
  wsP (string "=")
  z <- wsP callP
  return (AssignFunc (Val x) z)
  
assignRP :: Parser Char Statement
assignRP = do 
  x <- wsP varP
  wsP (string ":=")
  z <- wsP exprP
  return (AssignRef (Val x) z)

assignFRP :: Parser Char Statement
assignFRP = do 
  x <- wsP varP
  wsP (string ":=")
  z <- wsP callP
  return (AssignFuncRef (Val x) z)

skipP :: Parser Char Statement
skipP = do
  wsP (constP "skip" Skip)
  return Skip

whileP :: Parser Char Statement
whileP = do
  e <- wsP $ between (wsP (string "while")) (wsP exprP) (wsP (string "do"))
  s <- wsP statementP
  wsP (string "endwhile")
  return (While e s)
  
ifP :: Parser Char Statement
ifP = do
  e <- wsP $ between (wsP (string "if")) (wsP exprP) (wsP (string "then"))
  x <- wsP statementP
  y <- wsP $ between (wsP (string "else")) (wsP statementP) (wsP (string "endif"))
  return (If e x y)

sequenceP :: Parser Char Statement
sequenceP = do 
  s1 <- wsP leftSequenceP 
  wsP (char ';')
  s2 <- wsP statementP
  return (Sequence s1 s2)
  
-- @print "" X
-- @print "hello world"
-- @print "hello" X  
printP :: Parser Char Statement
printP = do
  wsP (string "@print")
  wsP (string "\"")
  str <- many (satisfy ('"' /=))
  wsP (string "\"")
  e <- wsP $ many exprP
  return $ Print str (aux e)
  where
    aux [] = Expr $ Val Null
    aux (x:_) = Expr x

throwP :: Parser Char Statement
throwP = do
  wsP (string "throw")
  e <- wsP exprP 
  return (Throw e)

tryP :: Parser Char Statement
tryP = do
  st1 <- wsP $ between (wsP (string "try")) (wsP statementP) (wsP (string "catch"))
  var <- wsP variableP
  st2 <- wsP $ between (wsP (string "with")) (wsP statementP) (wsP (string "endwith"))
  return (Try st1 var st2)

returnP :: Parser Char Statement
returnP = do
  wsP (string "return")
  e <- wsP exprP 
  return (Return e)

-- @fname (x,y,z)
callP :: Parser Char Statement
callP = do
  wsP (string "@")
  fname <- many alpha
  fargs <- wsP $ between (wsP (string "(")) (wsP $ ilistP exprP) (wsP (string ")"))
  return $ CallFunction fname fargs


-- Parse all the function declarations and return a FuncStore
funcmapP :: Parser Char FuncStore
funcmapP = do
  fname <- wsP $ many alpha
  fargs <- wsP $ between (wsP (string "(")) (wsP $ ilistP (exprP)) (wsP (string ")"))
  func <- wsP $ between (wsP (string "{")) (wsP statementP) (wsP (string "}"))
  return $ Map.insert fname (convert fargs, func) Map.empty 
    where
      convert [] = []
      convert ((Val (Var v)):es) = v:(convert es)
      convert _  = []

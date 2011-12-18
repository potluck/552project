-- Thanat Owlarn (towlarn@)
-- Pulak Mittal (pulak@)
-- CIS 552 hw6

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Main where

import WhilePP

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Test.HUnit hiding (State)

type Store = Map Variable Value

type FuncStore = Map String Function

mem_prefix :: String
mem_prefix = "mem"

lookupVar :: (MonadState ([Store], FuncStore) m, MonadError Value m, MonadWriter String m) => Variable -> m Value -- State Store Value
lookupVar v = do
  s <- get
  if Map.notMember v (head $ fst s) then 
    throwError (IntVal 0)
  else
    return $ Map.findWithDefault (IntVal 0) v (head $ fst s)  

evalE :: (MonadState ([Store], FuncStore) m, MonadError Value m, MonadWriter String m) => Expression -> m Value -- State Store Value

evalE (Val (Var v)) = lookupVar v
evalE (Val v) = do
  return v
evalE (Dereference v) = lookupVar v
evalE (Op bop e1 e2) = do
  e1' <- evalE e1 
  e2' <- evalE e2 
  case (e1', e2') of
    (IntVal x, IntVal y) -> 
      case bop of
        Plus    -> return (IntVal (x+y))
        Minus   -> return (IntVal (x-y))
        Times   -> return (IntVal (x*y))
        Divide  -> case y of 
          0 -> throwError (IntVal 1)
          _ -> return (IntVal (x `div` y))
        Gt      -> return (BoolVal (x>y))
        Ge      -> return (BoolVal (x>=y))
        Lt      -> return (BoolVal (x<y))
        Le      -> return (BoolVal (x<=y))
    -- TODO: Clean up this mess
    (IntVal x', DoubleVal y) -> 
      case bop of
        Plus    -> return (DoubleVal (x+y))
        Minus   -> return (DoubleVal (x-y))
        Times   -> return (DoubleVal (x*y))
        Divide  -> case y of 
          0 -> throwError (DoubleVal 1)
          _ -> return (DoubleVal (x / y))
        Gt      -> return (BoolVal (x>y))
        Ge      -> return (BoolVal (x>=y))
        Lt      -> return (BoolVal (x<y))
        Le      -> return (BoolVal (x<=y))    
        where x = fromIntegral x'
    (DoubleVal x, IntVal y') -> 
      case bop of
        Plus    -> return (DoubleVal (x+y))
        Minus   -> return (DoubleVal (x-y))
        Times   -> return (DoubleVal (x*y))
        Divide  -> case y of 
          0 -> throwError (DoubleVal 1)
          _ -> return (DoubleVal (x / y))
        Gt      -> return (BoolVal (x>y))
        Ge      -> return (BoolVal (x>=y))
        Lt      -> return (BoolVal (x<y))
        Le      -> return (BoolVal (x<=y))    
        where y = fromIntegral y'
    (DoubleVal x, DoubleVal y) -> 
      case bop of
        Plus    -> return (DoubleVal (x+y))
        Minus   -> return (DoubleVal (x-y))
        Times   -> return (DoubleVal (x*y))
        Divide  -> case y of 
          0 -> throwError (DoubleVal 1)
          _ -> return (DoubleVal (x / y))
        Gt      -> return (BoolVal (x>y))
        Ge      -> return (BoolVal (x>=y))
        Lt      -> return (BoolVal (x<y))
        Le      -> return (BoolVal (x<=y))    
    _ -> throwError (IntVal 2)


getVarFromValue :: (MonadState ([Store], FuncStore) m, MonadError Value m, MonadWriter String m) => Value -> m Variable
getVarFromValue (Var v) = return v
getVarFromValue _       = throwError (IntVal 3)

-- evalS :: Statement -> m Value
evalS :: (MonadState ([Store], FuncStore) m, MonadError Value m, MonadWriter String m) => Statement -> m Value

evalS st'@(While e st) = do
  e' <- evalE e -- m Value
  case e' of
    BoolVal b -> if b then evalS (Sequence st st') else evalS Skip
    Var v -> do
      v' <- lookupVar v
      case v' of 
        BoolVal b -> if b then evalS (Sequence st st') else evalS Skip
        _ -> throwError (IntVal 2)
    _  -> throwError (IntVal 2)

evalS Skip             = return Null

evalS (Sequence s1 s2) = evalS s1 >> evalS s2

evalS (Assign e1 e2) = 
  case e1 of 
    (Val (Var v)) -> do 
      s <- get
      e <- evalE e2
      let m = Map.insert v e (head $ fst s)
      put (m:(tail $ fst s), snd s)
      return e
    (Dereference v) -> do
      v' <- lookupVar v
      v'' <- getVarFromValue v'
      s <- get
      e <- evalE e2
      let m = Map.insert v'' e (head $ fst s)
      put (m:(tail $ fst s), snd s)
      return e
    _ -> throwError (IntVal 2)
    
evalS (AssignFunc e1 stmt) =
    case e1 of 
    (Val (Var v)) -> do 
      s <- get
      e <- evalS stmt
      let m = Map.insert v e (head $ fst s)
      put (m:(tail $ fst s), snd s)
      return e
    (Dereference v) -> do
      v' <- lookupVar v
      v'' <- getVarFromValue v'
      s <- get
      e <- evalS stmt
      let m = Map.insert v'' e (head $ fst s)
      put (m:(tail $ fst s), snd s)
      return e
    _ -> throwError (IntVal 2)
  
evalS (AssignRef e1 e2) = 
  case e1 of 
    (Val (Var v)) -> do 
      s <- get
      e <- evalE e2
      
      let m = Map.insert v (Var (mem_prefix ++ v)) (head $ fst s)
      let m2 = Map.insert (mem_prefix ++ v) e m
          
      put (m2:(tail $ fst s), snd s)
      return e
    _ -> throwError (IntVal 2)

evalS (If e s1 s2 )    = do
  e' <- evalE e
  case e' of
    BoolVal b -> if b then evalS s1 else evalS s2
    Var v -> do
      v' <- lookupVar v
      case v' of 
        BoolVal b -> if b then evalS s1 else evalS s2
        _ -> throwError (IntVal 2)
    _  -> throwError (IntVal 2)

evalS (Print s e) = do
  e' <- evalE e
  tell (s ++ display e')
  return Null

evalS (Throw e) = do 
  e' <- evalE e
  _  <- throwError e'
  return Null

evalS (Try s1 v s2) = 
  catchError (evalS s1)
             (\err -> 
               do m <- get
                  let m' = Map.insert v err (head $ fst m)
                  put (m':(tail $ fst m), snd m)
                  evalS s2
             )

-- Note: ALL functions should terminate with a return statement
-- Because this is where we pop the function stack
evalS (Return e) = do
  e' <- evalE e
  s <- get
  put (tail $ fst s, snd s)
  return e'

evalS (CallFunction fname vs) = do
  s <- get  
  let funcStore = snd s
  if Map.notMember fname funcStore then
    throwError (IntVal 3)
  else do
    let (args, stmt) = Map.findWithDefault dummyFunction fname funcStore
    if not $ length vs == length args then
       throwError (IntVal 4)
    else do
       -- marshall the arguments
       let localstore = marshallArgs args vs 

       -- push to function stack
       put (localstore:(fst s), funcStore)
       evalS stmt
    
-- Store = Map Variable Value
marshallArgs :: [String] -> [Value] -> Store
marshallArgs [] [] = Map.empty
marshallArgs (a:as) (s:ss) = Map.insert a s (marshallArgs as ss)
marshallArgs _ _ = error "Invalid Args"


dummyFunction :: Function
dummyFunction = ([], Skip)





instance Error Value   
type ESW a = ErrorT Value (WriterT String (State Store)) a

-- Errors 
-- 0: Reading undefined value
-- 1: Division by zero
-- 2: Runtime error (Add int to bool, comparison of non-ints)
-- 3: Function does not exist
-- 4: Invalid number of arguments passed to function

execute :: ([Store],FuncStore) -> Statement -> (([Store], FuncStore), Maybe Value, String)
execute store stmt = (store', result, lg) where
  ((res, lg), store') = runState (runWriterT (runErrorT (evalS stmt))) store
  result = case res of 
    Left s -> Just s
    Right _ -> Nothing


{-
raises :: Statement -> Value -> Test
s `raises` v = case (execute Map.empty s) of
    (_, Just v', _) -> v ~?= v'
    _  -> 1 ~?= 2
-}


{-
t1 :: Test
t1 = (Assign "X"  (Var "Y")) `raises` IntVal 0

t2 :: Test
t2 = (Assign "X" (Op Divide (Val (IntVal 1)) (Val (IntVal 0)))) `raises` IntVal 1

t3 :: Test       
t3 = TestList [ Assign "X" (Op Plus (Val (IntVal 1)) (Val (BoolVal True))) `raises` IntVal 2,      
                If (Val (IntVal 1)) Skip Skip `raises` IntVal 2,
                While (Val (IntVal 1)) Skip `raises` IntVal 2]

testprog1 :: Statement
testprog1 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Print "hello world: " $ Var "X",
                        If (Op Lt (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y")))
                                                       Skip,
                        Assign "Z" $ Val $ IntVal 3]

t4 :: Test
t4 = execute Map.empty testprog1 ~?=
  (Map.fromList [("X", IntVal 0), ("Y",  IntVal 1)], Just (IntVal 1), "hello world: 0")

testprog2 :: Statement
testprog2 = mksequence [Assign "X" $ Val $ IntVal 0,
                        Assign "Y" $ Val $ IntVal 1,
                        Try (If (Op Lt (Var "X") (Var "Y"))
                                (mksequence [Assign "A" $ Val $ IntVal 100,
                                             Throw (Op Plus (Var "X") (Var "Y")),
                                             Assign "B" $ Val $ IntVal 200])
                                Skip)
                            "E"
                            (Assign "Z" $ Op Plus (Var "E") (Var "A"))]

t5 :: Test
t5 = execute Map.empty testprog2 ~?=
   ( Map.fromList [("A", IntVal 100), ("E", IntVal 1)
          ,("X", IntVal 0), ("Y", IntVal 1)
          ,("Z", IntVal 101)]
          , Nothing 
   , "")
-}

-- Test cases

mksequence :: [Statement] -> Statement
mksequence = foldr Sequence Skip

make_var :: String -> Expression
make_var x = (Val $ Var x)

varX :: Expression
varX = make_var "X"

varY :: Expression
varY = make_var "Y"

varZ :: Expression
varZ = make_var "Z"



-- X := 3
-- Y = *X
testref1 :: Statement
testref1 = mksequence [ AssignRef varX (Val $ IntVal 3),
                        Assign varY (Dereference "X"),
                        Print "Y = " $ varY
                      ]

tr1 :: Test
tr1 = execute ([Map.empty], Map.empty) testref1 ~?=
  ( ([ Map.fromList [
          ("X", (Var $ mem_prefix++"X")), 
          ("Y",  IntVal 3), 
          (mem_prefix++"X", IntVal 3)] ], 
     Map.empty),
   Nothing, "Y = 3")
  

-- X := 3
-- Y = X
testref2 :: Statement
testref2 = mksequence [ AssignRef varX (Val $ IntVal 3),
                        Assign varY varX,
                        Print "Y = " $ varY
                      ]

tr2 :: Test
tr2 = execute ([Map.empty], Map.empty) testref2 ~?=
  ( ([ Map.fromList [
          ("X", (Var $ mem_prefix++"X")), 
          ("Y", (Var $ mem_prefix++"X")), 
          (mem_prefix++"X", IntVal 3)] ], 
     Map.empty),
   Nothing, "Y = "++mem_prefix++"X")

-- X := 3
-- Y = X
-- *X = 5
testref3 :: Statement
testref3 = mksequence [ AssignRef varX (Val $ IntVal 3),
                        Assign varY varX,
                        Assign (Dereference "X") (Val $ IntVal 5),
                        Print "Y = " $ (Dereference "Y")
                      ]

tr3 :: Test
tr3 = execute ([Map.empty], Map.empty) testref3 ~?=
  ( ([ Map.fromList [
          ("X", (Var $ mem_prefix++"X")), 
          ("Y", (Var $ mem_prefix++"X")), 
          (mem_prefix++"X", IntVal 5)] ], 
     Map.empty),
   Nothing, "Y = 5")


-- test_func(x):
--   y = x
--   return y
test_function :: Function
test_function = (
  ["X"], 
  mksequence [ Assign varY (Op Plus varX (Val $ IntVal 1)),
               Return varY ]
  )

testfunc1 :: Statement
testfunc1 = mksequence [
  Assign varX (Val $ IntVal 3),
  AssignFunc varY (CallFunction "foo" [(Var "X")]),
  Print "Y = " $ varY,
  Return varY
  ]


-- Not sure what the expected value should be. Need to review.
tf1 :: Test
tf1 = execute ([Map.empty], funcMap) testfunc1 ~?=
  ( ( [Map.fromList [
          ("X", (IntVal 3)), 
          ("Y", (IntVal 4))
          ] ],
     funcMap),
   Nothing, "Y = 4")
  
funcMap :: FuncStore  
funcMap = Map.fromList [("foo", test_function)]


main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ tr1, tr2, tr3, tf1 ] --t1, t2, t3, t4, t5
   return ()

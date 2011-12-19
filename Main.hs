-- Thanat Owlarn (towlarn@)
-- Pulak Mittal (pulak@)
-- CIS 552 hw6

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans -XMultiParamTypeClasses -XFlexibleContexts -XUndecidableInstances #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances #-}

module Main where

import WhilePP

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

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
evalE (Dereference v) = do
  v' <- lookupVar v
  evalE (Val v')
evalE (Op bop e1 e2) = do
  e1' <- case e1 of
              Stmt s -> evalS s
              Expr e -> evalE e 
  e2' <- case e2 of
              Stmt s -> evalS s
              Expr e -> evalE e
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

evalS Skip = return Null

evalS (Sequence s1 s2) = do 
  _ <- evalS s1
  evalS s2

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
    -- Case
    -- a := 3
    -- b := a
    -- #b := 5  ===  memB := 5
    (Dereference v) -> do
      v' <- lookupVar v
      v'' <- getVarFromValue v' -- v'' == memB
      s <- get
      e <- evalE e2
          
      let m = Map.insert v'' (Var (mem_prefix ++ v'')) (head $ fst s)
      let m2 = Map.insert (mem_prefix ++ v'') e m    
          
      put (m2:(tail $ fst s), snd s)
      return e
    _ -> throwError (IntVal 2)
    
    
evalS (AssignFuncRef e1 stmt) =
    case e1 of 
    -- Case  
    -- x := foo(3,5)  
    (Val (Var v)) -> do 
      s <- get
      e <- evalS stmt
          
      let m = Map.insert v (Var (mem_prefix ++ v)) (head $ fst s)
      let m2 = Map.insert (mem_prefix ++ v) e m
          
      put (m2:(tail $ fst s), snd s)
      return e
    -- Case  
    -- #x := foo(3,5)  ===  memX := foo(3,5)
    (Dereference v) -> do
      v' <- lookupVar v
      v'' <- getVarFromValue v'
      s <- get
      e <- evalS stmt
          
      let m = Map.insert v'' (Var (mem_prefix ++ v'')) (head $ fst s)
      let m2 = Map.insert (mem_prefix ++ v'') e m        
          
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

evalS (Print str eval) = do
  e' <- case eval of
    Stmt s -> evalS s
    Expr e -> evalE e 
  tell (str ++ display e')
  return e'

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

-- CallFunction String [Expression]
evalS (CallFunction fname es) = do
  s <- get  
  let funcStore = snd s
  if Map.notMember fname funcStore then
    throwError (IntVal 3)
  else do
    let (args, stmt) = Map.findWithDefault dummyFunction fname funcStore
    if not $ length es == length args then
       throwError (IntVal 4)
    else do
       es' <- sequence $ evalListE es
       -- marshall the arguments
       let localstore = marshallArgs args es'

       -- push to function stack
       put (localstore:(fst s), funcStore)
       evalS stmt

-- evalE :: Expression -> m Value
evalListE :: (MonadState ([Store], FuncStore) m, MonadError Value m, MonadWriter String m) => [Expression] -> [m Value] -- State Store Value
--evalListE es = map (evalE (Val)) es
evalListE [] = []
evalListE (e:es) = (evalE e):(evalListE es) -- map (evalE (Val)) es

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





-- Test cases

mksequence :: [Statement] -> Statement
mksequence = foldr Sequence Skip

-- Functions MUST end with a Return statement (not with Skip)
mksequencefunc :: [Statement] -> Statement
mksequencefunc [] = error "Invalid usage. cant be empty"
mksequencefunc [x] = x
mksequencefunc (x:xs) = Sequence x (mksequencefunc xs)

make_var :: String -> Expression
make_var x = (Val $ Var x)

varX :: Expression
varX = make_var "X"

varY :: Expression
varY = make_var "Y"

varZ :: Expression
varZ = make_var "Z"



-- X := 3
-- Y = #X
testref1 :: Statement
testref1 = mksequence [ AssignRef varX (Val $ IntVal 3),
                        Assign varY (Dereference "X"),
                        Print "Y = " $ Expr varY
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
                        Print "Y = " $ Expr varY
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
-- #X = 5
testref3 :: Statement
testref3 = mksequence [ AssignRef varX (Val $ IntVal 3),
                        Assign varY varX,
                        Assign (Dereference "X") (Val $ IntVal 5),
                        Print "Y = " $ Expr (Dereference "Y")
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
--   y = x + 1
--   return y
local_function1 :: Function
local_function1 = (
  ["X"], 
  mksequencefunc [ Assign varY (Op Plus (Expr varX) (Expr $ Val $ IntVal 1)),
               Print "Y = " $ Expr varY,
               Return varY ]
  )

testfunc1 :: Statement
testfunc1 = mksequencefunc [
  Assign varX (Val $ IntVal 3),
  AssignFunc varY (CallFunction "foo" [varX]),
  Print "; Y = " $ Expr varY
  ]

tf1 :: Test
tf1 = execute ([Map.empty], funcMap) testfunc1 ~?=
  ( ( [Map.fromList [
          ("X", (IntVal 3)), 
          ("Y", (IntVal 4))
          ] ],
     funcMap),
   Nothing, "Y = 4; Y = 4")
  
funcMap :: FuncStore
funcMap = Map.fromList [("foo", local_function1)]


-- foo2(Y):
--   X := Y
--   #X = #X + 1
--   return #X
local_function2 :: Function
local_function2 = (
  ["Y"], 
  mksequencefunc [ 
    AssignRef varX varY,
    Assign (Dereference "X") (Op Plus (Expr $ Dereference "X") (Expr $ Val $ IntVal 1)),
    Print "X = " $ Expr (Dereference "X"),
    Return (Dereference "X") ]
  )

-- Y = 3
-- X = foo2(Y)
testfunc2 :: Statement
testfunc2 = mksequencefunc [
  Assign varY (Val $ IntVal 3),
  AssignFunc varX (CallFunction "foo" [varY]),
  Print "; X = " $ Expr varX
  ]

tf2 :: Test
tf2 = execute ([Map.empty], funcMap2) testfunc2 ~?=
  ( ( [Map.fromList [
          ("X", (IntVal 4)), 
          ("Y", (IntVal 3))
          ] ],
     funcMap2),
   Nothing, "X = 4; X = 4")
  
funcMap2 :: FuncStore
funcMap2 = Map.fromList [("foo", local_function2)]

-- Y := 3
-- X := foo2(#Y)
testfunc2b :: Statement
testfunc2b = mksequencefunc [
  AssignRef varY (Val $ IntVal 3),
  AssignFuncRef varX (CallFunction "foo" [(Dereference "Y")]),
  Print "; X = " $ Expr (Dereference "X")
  ]

tf2b :: Test
tf2b = execute ([Map.empty], funcMap2) testfunc2b ~?=
  ( ( [Map.fromList [
          ("X", (Var $ mem_prefix++"X")), 
          ("Y", (Var $ mem_prefix++"Y")),
          ((mem_prefix++"Y"), IntVal 3),
          ((mem_prefix++"X"), IntVal 4)          
          ] ],
     funcMap2),
   Nothing, "X = 4; X = 4")

-- Y := 3
-- X := Null
-- Z := X
-- print #Z
testfunc2c :: Statement
testfunc2c = mksequencefunc [
  AssignRef varY (Val $ IntVal 3),
  AssignRef varX (Val Null),
  AssignRef varZ varX,
  Print "Z = " $ Expr (Dereference "Z") -- give us memX
  ]

tf2c :: Test
tf2c = execute ([Map.empty], funcMap2) testfunc2c ~?=
  ( ( [Map.fromList [
          ("X", (Var $ mem_prefix++"X")), 
          ("Y", (Var $ mem_prefix++"Y")),
          ("Z", (Var $ mem_prefix++"Z")),
          ((mem_prefix++"Y"), IntVal 3),
          ((mem_prefix++"X"), Null),
          ((mem_prefix++"Z"), (Var $ mem_prefix++"X"))
          ] ],
     funcMap2),
   Nothing, "Z = memX")


-- Y := 3
-- X := Null
-- Z := X
-- #Z := foo2(#Y)
-- print ##Z
testfunc2d :: Statement
testfunc2d = mksequencefunc [
  AssignRef varY (Val $ IntVal 3),
  AssignRef varX (Val Null),
  AssignRef varZ varX,
  AssignFuncRef (Dereference "Z") (CallFunction "foo" [(Dereference "Y")]),
  Print "; Z = " $ Expr (Dereference $ mem_prefix++"Z") -- give us ##Z
  ]

tf2d :: Test
tf2d = execute ([Map.empty], funcMap2) testfunc2d ~?=
  ( ( [Map.fromList [
          ("X", (Var $ mem_prefix++"X")), 
          ("Y", (Var $ mem_prefix++"Y")),
          ("Z", (Var $ mem_prefix++"Z")),
          ((mem_prefix++"Y"), IntVal 3),
          ((mem_prefix++"X"), Null),
          ((mem_prefix++"Z"), (Var $ mem_prefix++mem_prefix++"Z")),
          ((mem_prefix++mem_prefix++"Z"), IntVal 4)
          ] ],
     funcMap2),
   Nothing, "X = 4; Z = 4")


-- foo3(X, Y):
--   return foo1(X) * foo2(Y)
-- both foo1 and foo2 increments by 1
local_function3 :: Function
local_function3 = (
  ["X", "Y"], 
  mksequencefunc [ 
    Return (Op Times (Stmt (CallFunction "foo1" [varX])) (Stmt (CallFunction "foo2" [varY]))) ]
  )
                  
--                   
-- print foo3(3.5,5)
testfunc3 :: Statement
testfunc3 = mksequencefunc [
  Print "" $ Stmt (CallFunction "foo3" [(Val $ DoubleVal 3.5), (Val $ IntVal 5)])
  ]
            
            
tf3 :: Test
tf3 = execute ([Map.empty], funcMap3) testfunc3 ~?=
  ( ( [Map.fromList [
          ] ],
     funcMap3),
   Nothing, "Y = 4.5X = 627.0")
                  
funcMap3 :: FuncStore
funcMap3 = Map.fromList [("foo1", local_function1),("foo2", local_function2),("foo3", local_function3)] 
                  

print_functions :: IO ()
print_functions = do
  putStrLn $ display testref1
  putStrLn $ display testref2
  putStrLn $ display testref3
  putStrLn $ display testfunc1
  putStrLn $ display testfunc2
  putStrLn $ display testfunc2b
  putStrLn $ display testfunc2c
--  putStrLn $ display local_function1
--  putStrLn $ display local_function2
--  putStrLn $ display local_function3

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ tr1, tr2, tr3, tf1, tf2, tf2b, tf2c, tf2d, 
                               tf3 ]
   return ()
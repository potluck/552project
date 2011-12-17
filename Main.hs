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


evalE :: (MonadState [Store] m, MonadError Value m, MonadWriter String m) => Expression -> m Value -- State Store Value
evalE (Var v) = do
  s <- get
  if Map.notMember v (head s) then 
    throwError (IntVal 0)
  else
    return $ Map.findWithDefault (IntVal 0) v (head s)
evalE (Val v) = do
  return v
evalE (Op bop e1 e2) = do
  e1' <- evalE e1 
  e2' <- evalE e2 
--  let e1' = evalState (evalE e1) s
--  let e2' = evalState (evalE e2) s
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
    _ -> throwError (IntVal 2)



evalS :: (MonadState Store m, MonadError Value m, MonadWriter String m) => Statement -> m ()
evalS = undefined
{-
evalS st'@(While e st) = do
  e' <- evalE e -- m Value
  case e' of
    IntVal _  -> throwError (IntVal 2)
    BoolVal b -> if b then evalS (Sequence st st') else evalS Skip
evalS Skip             = return ()
evalS (Sequence s1 s2) = evalS s1 >> evalS s2
evalS (Assign v e)     = do
  s  <- get
  e' <- evalE e
  let m = Map.insert v e' s
  put m
evalS (If e s1 s2 )    = do
  e' <- evalE e
  case e' of
    IntVal _  -> throwError (IntVal 2)
    BoolVal b -> if b then evalS s1 else evalS s2
evalS (Print s e) = do
  e' <- evalE e
  tell (s ++ display e')
evalS (Throw e) = do 
  e' <- evalE e
  throwError e'
evalS (Try s1 v s2) = 
  catchError (evalS s1)
             (\err -> 
               do m <- get
                  let m' = Map.insert v err m
                  put m'
                  evalS s2
             ) 
-}

-- MonadState, MonadWriter, MonadError
instance Error Value   
type ESW a = ErrorT Value (WriterT String (State Store)) a

-- Errors 
-- 0: Reading undefined value
-- 1: Division by zero
-- 2: Runtime error (Add int to bool, comparison of non-ints)

execute :: Store -> Statement -> (Store, Maybe Value, String)
execute store stmt = (store', result, lg) where
  ((res, lg), store') = runState (runWriterT (runErrorT (evalS stmt))) store
  result = case res of 
    Left s -> Just s
    Right _ -> Nothing

{-
evalESW :: Statement -> ESW ()
evalESW x = evalS x

instance Show a => Show (ESW a) where 
  show m = "Log:\n"  ++ lg ++ "\n" ++ 
           "Store: " ++ show store ++ "\n" ++
           result
    where ((res, lg), store) = runState (runWriterT (runErrorT m)) Map.empty
          result = case res of 
                       Left (IntVal s) -> "Error: " ++ show s 
                       Left (BoolVal _) -> "Error in Error" 
                       Right v -> "Value: " ++ show v
-}

raises :: Statement -> Value -> Test
s `raises` v = case (execute Map.empty s) of
    (_, Just v', _) -> v ~?= v'
    _  -> 1 ~?= 2

t1 :: Test
t1 = (Assign "X"  (Var "Y")) `raises` IntVal 0

t2 :: Test
t2 = (Assign "X" (Op Divide (Val (IntVal 1)) (Val (IntVal 0)))) `raises` IntVal 1

t3 :: Test       
t3 = TestList [ Assign "X" (Op Plus (Val (IntVal 1)) (Val (BoolVal True))) `raises` IntVal 2,      
                If (Val (IntVal 1)) Skip Skip `raises` IntVal 2,
                While (Val (IntVal 1)) Skip `raises` IntVal 2]

mksequence :: [Statement] -> Statement
mksequence = foldr Sequence Skip

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

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ t1, t2, t3, t4, t5 ]
   return ()

module Interpreter.InterpreterUtils where

import qualified AbsGramar as G
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe, Maybe(..))
import Control.Monad.Except
import Control.Exception (Exception)

import Interpreter.InterpreterEnv

data MyError = ErrorUninitializedVariable String G.BNFC'Position
             | ErrorDevisionByZero G.BNFC'Position

printPos Nothing = ""
printPos (Just (l,c)) = " at line " ++ show l ++ ", column " ++ show c

instance Show MyError where
  show (ErrorUninitializedVariable name pos) = "UninitializedError\n unitialized variable " ++ name ++  printPos pos
  show (ErrorDevisionByZero pos) = "DevisionByZeroError \n devision by zero" ++ printPos pos

-- rezerwacja nowej lokacji
newloc :: Result Loc
newloc = do
  (st,l,f) <- get
  put (st,l+1,f)
  return l

-- funkcja pomocnicza do zmiany pamięci
-- takie modify, które zmienia tylko część stanu 
modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f =
  modify (\(st,l, fc) -> (f st,l, fc))

modifyFuncMem :: (FuncMem -> FuncMem) -> Result ()
modifyFuncMem f =
  modify (\(st,l,func) -> (st,l,f func))

incResult :: TypeOfResult -> TypeOfResult
incResult (MyInt n) = MyInt (n + 1)

decResult :: TypeOfResult -> TypeOfResult
decResult (MyInt n) = MyInt (n - 1)

prettyPrint :: [TypeOfResult] -> String
prettyPrint [] = ""
prettyPrint (x:xs) = case x of
                MyInt i -> show i ++ prettyPrint xs
                MyBool i -> show i ++ prettyPrint xs
                MyStr i -> i ++ prettyPrint xs
                MyVoid -> "void"

first (a, b,c) = a

transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transAddOp :: G.AddOp -> Result (Integer -> Integer -> Integer) 
transAddOp x = case x of
  G.Plus _ -> return (+)
  G.Minus _ -> return (-)

transMulOp :: G.MulOp -> Result ((Integer -> Integer -> Integer), Bool)
transMulOp x = case x of
  G.Times _ -> return ((*), False)
  G.Div _ -> return (div, True)
  G.Mod _ -> return (mod, False)

transRelOp :: G.RelOp -> Result(Integer -> Integer -> Bool)
transRelOp x = case x of
  G.LTH _ -> return (<)
  G.LE _ -> return (<=)
  G.GTH _ -> return (>)
  G.GE _ -> return (>=)
  G.EQU _ -> return (==)
  G.NE _ -> return (/=)
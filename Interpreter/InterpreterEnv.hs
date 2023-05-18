module Interpreter.InterpreterEnv where

import qualified AbsGramar as G
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data TypeOfResult = MyInt Integer 
                  | MyStr String
                  | MyBool Bool
                  | MyVoid
                  | MyBreak
                  | MyContinue
                  | MyFunc G.FnDef Env

type Loc = Int

data Env = Env { 
    varEnv :: M.Map String Loc
}

type Mem  = M.Map Loc TypeOfResult
type FuncMem = M.Map String (G.FnDef, Env) -- funkcje globalne
type Store = (Mem, Loc, FuncMem)

type Result a = ExceptT String (StateT Store (ReaderT Env IO)) a
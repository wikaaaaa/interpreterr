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
  --deriving (Show)

-- zbior globalnych funckji, i jesli nie istnieje lokalna funckja o danej nazwie to wyboerz globalna 

type Loc = Int -- lokacje pamieci

data Env = Env { 
    varEnv :: M.Map String Loc
}

type Mem  = M.Map Loc TypeOfResult
type FuncMem = M.Map String (G.FnDef, Env)
type Store = (Mem, Loc, FuncMem)
--type Store = (Mem, Loc)

type Result a = ExceptT String (StateT Store (ReaderT Env IO)) a
module TypeChecker.TypeCheckerEnv where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Set as Set

-- MyFunc Type [Type] Bool ->  (Bool == True) oznacza, ze funkcja jest funkcją globalną
data Type = MyInt | MyBool | MyStr | MyVoid | MyFunc Type [Type] Bool | MyRef Type | MyNothing
    deriving(Eq)

data Env = Env { 
    varType :: M.Map String Type, 
    inWhile :: Bool,
    names :: Set.Set String
}

type Result a = ExceptT String (Reader Env) a
module Interpreter where

import qualified AbsGramar as G
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe)
import Control.Monad.Except

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span, Show,  Either(..), IO, show, error, undefined, putStrLn, fst, putStr
  )

data TypeOfResult = MyInt Integer 
                  | MyStr String
                  | MyBool Bool
  deriving (Show)

type Loc = Int -- lokacje pamieci
type Env = M.Map String Loc
type Mem  = M.Map Loc TypeOfResult
type Store = (Mem, Loc)

type Result a = ExceptT String (StateT Store (ReaderT Env IO)) a

failure :: Show a => a -> Result ()
failure x = throwError $ "x " ++ show x

-- rezerwacja nowej lokacji
newloc :: Result Loc
newloc = do
  (st,l) <- get
  put (st,l+1)
  return l

-- funkcja pomocnicza do zmiany pamięci
-- takie modify, które zmienia tylko część stanu 
modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f =
  modify (\(st,l) -> (f st,l))

transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transProgram :: Show a => G.Program' a -> Result()
transProgram x = case x of
  G.Program _ topdefs -> transTopDefs topdefs

transTopDefs :: Show a => [G.TopDef' a] -> Result ()
transTopDefs [] = return ()
transTopDefs (y:ys) = case y of

    G.FnDef _ type_ ident args block -> transBlock block

    G.VarDef _ type_ item -> case item of
      G.NoInit _ ident -> do
        l <- newloc
        id <- transIdent ident
        let i = MyInt 0
        modifyMem (M.insert l i)
        local (M.insert id l) (transTopDefs ys)


      G.Init _ ident expr -> do
        e <- transExpr expr
        l <- newloc
        id <- transIdent ident
        modifyMem (M.insert l e)
        local (M.insert id l) (transTopDefs ys)

{-transTopDef :: Show a => G.TopDef' a -> Result
transTopDef x = case x of
  G.FnDef _ type_ ident args block -> failure x
  G.VarDef _ type_ item -> failure x
-}

transArg :: Show a => G.Arg' a -> Result ()
transArg x = case x of
  G.Arg _ type_ ident -> failure x
  G.ArgRef _ type_ ident -> failure x

transBlock :: Show a => G.Block' a -> Result()
transBlock x = case x of
  G.Block _ stmts -> failure x

transStmt :: Show a => G.Stmt' a -> Result()
transStmt x = case x of
  G.Empty _ -> failure x
  G.BStmt _ block -> failure x
  G.Decl _ topdef -> failure x
  G.Ass _ ident expr -> failure x
  G.Incr _ ident -> failure x
  G.Decr _ ident -> failure x
  G.Ret _ expr -> failure x
  G.VRet _ -> failure x
  G.Cond _ expr block -> failure x
  G.CondElse _ expr block1 block2 -> failure x
  G.While _ expr block -> failure x
  G.SExp _ expr -> failure x
  G.Break _ -> failure x
  G.Continue _ -> failure x

transItem :: Show a => G.Item' a -> Result()
transItem x = case x of
  G.NoInit _ ident -> failure x
  G.Init _ ident expr -> failure x

transType :: Show a => G.Type' a -> Result()
transType x = case x of
  G.MyInt _ -> failure x
  G.MyStr _ -> failure x
  G.MyBool _ -> failure x
  G.MyVoid _ -> failure x

transExpr :: Show a => G.Expr' a -> Result TypeOfResult
transExpr x = case x of
  G.EVar _ ident -> do
              env <- ask
              id <-  transIdent ident
              let l = fromMaybe (error "undefined variable") (M.lookup id env)
              (st,_)  <- get
              return $ fromMaybe (error "undefined location") (M.lookup l st)

  G.ELitInt _ integer -> return $ MyInt integer
  G.ELitTrue _ -> return $ MyBool True
  G.ELitFalse _ -> return $ MyBool False
  G.EApp _ ident exprs -> undefined
  G.EString _ string -> return $ MyStr string
  G.Neg _ expr -> undefined
  G.Not _ expr -> undefined
  G.EMul _ expr1 mulop expr2 -> undefined

  G.EAdd _ expr1 addop expr2 -> do
                    MyInt e1 <- transExpr expr1
                    MyInt e2 <- transExpr expr2
                    return $ MyInt (transAddOp addop e1 e2)

  G.ERel _ expr1 relop expr2 -> undefined
  G.EAnd _ expr1 expr2 -> undefined
  G.EOr _ expr1 expr2 -> undefined

transAddOp :: Show a => G.AddOp' a -> (Integer -> Integer -> Integer) -- gubię Result, czy to źle?
transAddOp x = case x of
  G.Plus _ -> (+)
  G.Minus _ -> (-)

transMulOp :: Show a => G.MulOp' a -> Result()
transMulOp x = case x of
  G.Times _ -> failure x
  G.Div _ -> failure x
  G.Mod _ -> failure x

transRelOp :: Show a => G.RelOp' a -> Result()
transRelOp x = case x of
  G.LTH _ -> failure x
  G.LE _ -> failure x
  G.GTH _ -> failure x
  G.GE _ -> failure x
  G.EQU _ -> failure x
  G.NE _ -> failure x

interpret :: G.Program -> Result () 
interpret = transProgram

-- Ignore function definitions and just execute block of statements
-- exec (MT.Prog _ (MT.Func _ (MT.FuncDefStmt _ _ _ _ (MT.Blck _ stmts)) : _)) = interpret stmt



runInterpreter program = 
    let (newEnv, newState) = (M.empty, (M.empty, 0))
    in do 
    (res, a) <- runReaderT (runStateT (runExceptT (interpret program)) newState) newEnv
    case res of
      Left e -> putStrLn $ "runtime error: " ++ e
      Right f -> mapM_ wypisz $ M.toList $ fst $ a
          where 
            wypisz (l, i) = do {putStr $ (show l)++", "; putStrLn $ show i}  
      
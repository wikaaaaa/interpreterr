module Interpreter where

import qualified AbsGramar as G
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe)
import Control.Monad.Except

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<), (<=), (>), (>=), (/=), not, (&&), (||)
  , Int, Integer, Double, (+), (-), (*), (/)
  , String, (++), print
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span, Show,  Either(..), IO, show, error, undefined, putStrLn, fst, putStr
  )

data TypeOfResult = MyInt Integer 
                  | MyStr String
                  | MyBool Bool
  deriving (Show)



type Loc = Int -- lokacje pamieci
type Env = M.Map String Loc
type Funcs = M.Map String ??
type Mem  = M.Map Loc TypeOfResult
type Store = (Mem, Loc, Funcs)

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

    G.FnDef _ type_ ident args block -> do
        id <- transIdent ident
        case id of
          "main" -> transBlock block >> transTopDefs ys
          _ -> transTopDefs ys -- olewam na razie funkcje inne niz main
     

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

{-
transTopDef :: Show a => G.TopDef' a -> Result
transTopDef x = case x of
  G.FnDef _ type_ ident args block -> undefined

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
-}

transArg :: Show a => G.Arg' a -> Result ()
transArg x = case x of
  G.Arg _ type_ ident -> failure x
  G.ArgRef _ type_ ident -> failure x

transBlock :: Show a => G.Block' a -> Result()
transBlock x = case x of
  G.Block _ stmts -> transStmts stmts -- tu jakis local chyba

-- typechekcer zapewnia ze tu bedzie int
incResult :: TypeOfResult -> TypeOfResult
incResult (MyInt n) = MyInt (n + 1)

decResult :: TypeOfResult -> TypeOfResult
decResult (MyInt n) = MyInt (n - 1)


transStmts :: Show a => [G.Stmt' a] -> Result()
transStmts [] = return ()
transStmts (x:xs) = case x of

  G.Empty _ -> return ()

  G.BStmt _ block -> failure x
  
  G.Decl _ topdef -> case topdef of
    
      G.FnDef _ type_ ident args block -> undefined

      G.VarDef _ type_ item -> case item of
          G.NoInit _ ident -> do
            l <- newloc
            id <- transIdent ident
            let i = MyInt 0
            modifyMem (M.insert l i)
            local (M.insert id l) (transStmts xs)

          G.Init _ ident expr -> do
            e <- transExpr expr
            l <- newloc
            id <- transIdent ident
            modifyMem (M.insert l e)
            local (M.insert id l) (transStmts xs)

  -- zakładając ze zmienna byla wczesniej zadeklarowana
  G.Ass _ ident expr -> do
          env <- ask
          id <- transIdent ident
          let l = fromMaybe (error "undefined variable") (M.lookup id env) -- ten błąd powinien zglaszac typechecker
          w <- transExpr expr
          modifyMem $ M.insert l w
          transStmts xs

  G.Incr _ ident -> do
          env <- ask
          id <- transIdent ident
          let l = fromMaybe (error "undefined variable") (M.lookup id env) -- ten błąd powinien zglaszac typechecker
          modifyMem $ \mem -> M.adjust incResult l mem
          transStmts xs


  G.Decr _ ident -> do
          env <- ask
          id <- transIdent ident
          let l = fromMaybe (error "undefined variable") (M.lookup id env) -- ten błąd powinien zglaszac typechecker
          modifyMem $ \mem -> M.adjust decResult l mem
          transStmts xs

  G.Ret _ expr -> failure x
  G.VRet _ -> failure x

  G.Cond _ expr block -> do
          MyBool e <- transExpr expr
          if e==True then transBlock block >> transStmts xs else transStmts xs


  G.CondElse _ expr block1 block2 -> do
          MyBool e <- transExpr expr
          if e==True then transBlock block1 >> transStmts xs else transBlock block2 >> transStmts xs

  G.While pos expr block -> do
          MyBool w <- transExpr expr
          if w==False then transStmts xs else transBlock block >> transStmts ((G.While pos expr block):xs)

  G.SExp _ expr -> transExpr expr >> transStmts xs -- aplikacja funkcji

  G.Break _ -> undefined
  G.Continue _ -> undefined

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

  G.EApp _ ident exprs -> do
                e <- mapM transExpr exprs
                id <- transIdent ident
                case id of
                  "print" -> do
                            liftIO $ mapM_ print e
                            return $ MyBool True
                  _ -> undefined
                    
  G.EString _ string -> return $ MyStr string

  G.Neg _ expr -> do
                MyInt e <- transExpr expr
                return $ MyInt (-1 * e)

  G.Not _ expr -> do
                MyBool e <- transExpr expr
                return $ MyBool (not e)
  
  G.EMul _ expr1 mulop expr2 -> do
                    MyInt e1 <- transExpr expr1
                    MyInt e2 <- transExpr expr2
                    op <- transMulOp mulop
                    return $ MyInt (op e1 e2)

  G.EAdd _ expr1 addop expr2 -> do
                    MyInt e1 <- transExpr expr1
                    MyInt e2 <- transExpr expr2
                    op <- transAddOp addop
                    return $ MyInt (op e1 e2)

  G.ERel _ expr1 relop expr2 ->  do
                    MyInt e1 <- transExpr expr1
                    MyInt e2 <- transExpr expr2
                    op <- transRelOp relop
                    return $ MyBool (op e1 e2)

  G.EAnd _ expr1 expr2 -> do
                    MyBool e1 <- transExpr expr1
                    MyBool e2 <- transExpr expr2
                    return $ MyBool (e1 && e2)

  G.EOr _ expr1 expr2 -> do
                    MyBool e1 <- transExpr expr1
                    MyBool e2 <- transExpr expr2
                    return $ MyBool (e1 || e2)

transAddOp :: Show a => G.AddOp' a -> Result (Integer -> Integer -> Integer) 
transAddOp x = case x of
  G.Plus _ -> return (+)
  G.Minus _ -> return (-)

transMulOp :: Show a => G.MulOp' a -> Result (Integer -> Integer -> Integer)
transMulOp x = case x of
  G.Times _ -> return (*)
  G.Div _ -> undefined
  G.Mod _ -> undefined

transRelOp :: Show a => G.RelOp' a -> Result(Integer -> Integer -> Bool)
transRelOp x = case x of
  G.LTH _ -> return (<=)
  G.LE _ -> return (<)
  G.GTH _ -> return (>=)
  G.GE _ -> return (>)
  G.EQU _ -> return (==)
  G.NE _ -> return (/=)

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
      
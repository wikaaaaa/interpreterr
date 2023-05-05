module Interpreter where

import qualified AbsGramar as G
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe, Maybe(..))
import Control.Monad.Except
import Control.Exception (Exception)

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
type Mem  = M.Map Loc TypeOfResult
type FuncMem = M.Map String (G.FnDef, Env)
type Store = (Mem, Loc, FuncMem)
--type Store = (Mem, Loc)

type Result a = ExceptT String (StateT Store (ReaderT Env IO)) a

failure :: Show a => a -> Result ()
failure x = throwError $ "x " ++ show x

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

transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transProgram ::  G.Program -> Result()
transProgram x = case x of
  G.Program _ topdefs -> transTopDefs topdefs

transTopDefs :: [G.TopDef] -> Result ()
transTopDefs [] = return ()
transTopDefs (y:ys) = case y of

    G.Fn _ fndef -> transFnDef fndef >> transTopDefs ys

     
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

transFnDef :: G.FnDef -> Result()
transFnDef x = case x of
  G.FnDef _ type_ ident args block -> do
        id <- transIdent ident
        env <- ask
        case id of
          "main" -> transBlock block >> return ()
          _ -> do
            modifyFuncMem(M.insert id (x, env))


transArg :: G.Arg -> Result ()
transArg x = case x of
  G.Arg _ type_ ident -> failure x
  G.ArgRef _ type_ ident -> failure x

transBlockWithRet ::  G.Block -> Result TypeOfResult
transBlockWithRet x = case x of
  G.Block _ stmts -> do
    Just ret <- transStmts stmts -- tu jakis local chyba
    return ret

transBlock ::  G.Block -> Result ()
transBlock x = case x of
  G.Block _ stmts -> transStmts stmts >> return ()

-- typechekcer zapewnia ze tu bedzie int
incResult :: TypeOfResult -> TypeOfResult
incResult (MyInt n) = MyInt (n + 1)

decResult :: TypeOfResult -> TypeOfResult
decResult (MyInt n) = MyInt (n - 1)


transStmts ::  [G.Stmt ] -> Result (Maybe TypeOfResult)
transStmts [] = return Nothing
transStmts (x:xs) = case x of

  G.Empty _ -> return Nothing

  G.BStmt _ block -> return Nothing
  
  G.Decl _ topdef -> case topdef of
    
      G.Fn _ fndef -> undefined

      G.VarDef _ type_ item -> case item of
          G.NoInit _ ident -> do
            l <- newloc
            id <- transIdent ident
            let i = MyInt 0
            modifyMem (M.insert l i)
            local (M.insert id l) (transStmts xs)
            return Nothing

          G.Init _ ident expr -> do
            e <- transExpr expr
            l <- newloc
            id <- transIdent ident
            modifyMem (M.insert l e)
            local (M.insert id l) (transStmts xs)
            return Nothing

  -- zakładając ze zmienna byla wczesniej zadeklarowana
  G.Ass _ ident expr -> do
          env <- ask
          id <- transIdent ident
          let l = fromMaybe (error "undefined variable") (M.lookup id env) -- ten błąd powinien zglaszac typechecker
          w <- transExpr expr
          modifyMem $ M.insert l w
          transStmts xs
          return Nothing

  G.Incr _ ident -> do
          env <- ask
          id <- transIdent ident
          let l = fromMaybe (error "undefined variable") (M.lookup id env) -- ten błąd powinien zglaszac typechecker
          modifyMem $ \mem -> M.adjust incResult l mem
          transStmts xs
          return Nothing


  G.Decr _ ident -> do
          env <- ask
          id <- transIdent ident
          let l = fromMaybe (error "undefined variable") (M.lookup id env) -- ten błąd powinien zglaszac typechecker
          modifyMem $ \mem -> M.adjust decResult l mem
          transStmts xs
          return Nothing

  G.Ret _ expr -> do 
            e <- transExpr expr
            return $ Just e
          
  G.VRet _ -> return $ Just (MyBool True)

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

transItem ::  G.Item -> Result()
transItem x = case x of
  G.NoInit _ ident -> failure x
  G.Init _ ident expr -> failure x

transType ::  G.Type -> Result()
transType x = case x of
  G.MyInt _ -> failure x
  G.MyStr _ -> failure x
  G.MyBool _ -> failure x
  G.MyVoid _ -> failure x

-- co z funkcjami ktore cos zwracaja? jak tą wartosc tam wrócic, na razie zajmijmy sie void
-- i olewam argumenty
transAppFunc :: G.FnDef -> [G.Expr] -> Env -> Result TypeOfResult
transAppFunc x expr env = case x of
  G.FnDef _ type_ ident args block -> local (\e -> env) (doFunc args expr block)

-- typechecker bedzie sprawdzal czy podane expr sa dobrego typu
--insertArg [] [] = local () ()
doFunc :: [G.Arg] -> [G.Expr] -> G.Block -> Result TypeOfResult
doFunc [] [] block =  transBlockWithRet block
doFunc (arg:args) (expr:exprs) block = do
              e <- transExpr expr
              case arg of

                G.Arg _ type_ ident -> do
                              id <- transIdent ident
                              l <- newloc
                              modifyMem (M.insert l e)
                              local (M.insert id l) (doFunc args exprs block)

                G.ArgRef _ type_ ident -> undefined

transExpr ::  G.Expr -> Result TypeOfResult
transExpr x = case x of
  G.EVar _ ident -> do
              env <- ask
              id <-  transIdent ident
              let l = fromMaybe (error "undefined variable") (M.lookup id env)
              (st,_,_)  <- get
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
                  _ -> do
                    (st,l,funcMem) <- get
                    let (fun, env) = fromMaybe (error "undefined function") (M.lookup id funcMem)
                    transAppFunc fun exprs env
                     -- zmienic localnie srodowisko na sprzed deklaracji funkcji 
                    
                    
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

transAddOp :: G.AddOp -> Result (Integer -> Integer -> Integer) 
transAddOp x = case x of
  G.Plus _ -> return (+)
  G.Minus _ -> return (-)

transMulOp :: G.MulOp -> Result (Integer -> Integer -> Integer)
transMulOp x = case x of
  G.Times _ -> return (*)
  G.Div _ -> undefined
  G.Mod _ -> undefined

transRelOp :: G.RelOp -> Result(Integer -> Integer -> Bool)
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

first (a, b,c) = a

runInterpreter program = 
    let (newEnv, newState) = (M.empty, (M.empty, 0, M.empty))
    in do 
    (res, a) <- runReaderT (runStateT (runExceptT (interpret program)) newState) newEnv
    case res of
      Left e -> putStrLn $ "runtime error: " ++ e
      Right f -> mapM_ wypisz $ M.toList $ first $ a
          where 
            wypisz (l, i) = do {putStr $ (show l)++", "; putStrLn $ show i}  
      
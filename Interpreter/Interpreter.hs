module Interpreter.Interpreter where

import qualified AbsGramar as G
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(Maybe(..))
import Control.Monad.Except
import Control.Exception (Exception)
import Interpreter.InterpreterEnv
import Interpreter.InterpreterUtils

transProgram ::  G.Program -> Result()
transProgram (G.Program _ topdefs) = transTopDefs topdefs

transTopDefs :: [G.TopDef] -> Result ()
transTopDefs [] = return ()
transTopDefs (y:ys) = case y of

    G.Fn _ fndef -> transFnDef fndef >> transTopDefs ys

    G.VarDef _ type_ item -> case item of
        G.NoInit _ ident -> do
            l <- newloc
            id <- transIdent ident
            local (\e -> e { varEnv = M.insert id l (varEnv e) } ) (transTopDefs ys)

        G.Init _ ident expr -> do
            e <- transExpr expr
            l <- newloc
            id <- transIdent ident
            modifyMem (M.insert l e)
            local (\e -> e { varEnv = M.insert id l (varEnv e) } ) (transTopDefs ys)

-- dodaje globalną funkcję
transFnDef :: G.FnDef -> Result()
transFnDef x = case x of

    G.FnDef _ type_ ident args block -> do
        id <- transIdent ident
        env <- ask
        case id of
            "main" -> transBlock block >> return ()
            _ -> modifyFuncMem(M.insert id (x, env))

transBlock ::  G.Block -> Result TypeOfResult
transBlock (G.Block _ stmts) = transStmts stmts

transStmts ::  [G.Stmt ] -> Result TypeOfResult
transStmts [] = return MyVoid
transStmts (x:xs) = case x of

    G.Empty _ -> return MyVoid
    
    G.Decl _ topdef -> case topdef of

        G.Fn _ func -> case func of

            G.FnDef _ type_ ident args block -> do
                id <- transIdent ident
                env <- ask
                let new_func = MyFunc func env
                l <- newloc
                modifyMem(M.insert l new_func)
                local (\e -> e { varEnv = M.insert id l (varEnv e) } ) (transStmts xs)

        G.VarDef _ type_ item -> case item of

            G.NoInit _ ident -> do
                l <- newloc
                id <- transIdent ident
                local (\e -> e { varEnv = M.insert id l (varEnv e) } ) (transStmts xs)

            G.Init _ ident expr -> do
                e <- transExpr expr
                l <- newloc
                id <- transIdent ident
                modifyMem (M.insert l e)
                local (\e -> e { varEnv = M.insert id l (varEnv e) } ) (transStmts xs)

    G.Ass _ ident expr -> do
        env <- ask
        id <- transIdent ident
        let Just l = M.lookup id (varEnv env)
        w <- transExpr expr
        modifyMem $ M.insert l w
        transStmts xs

    G.Incr pos ident -> do
        env <- ask
        id <- transIdent ident
        let Just l = M.lookup id (varEnv env)
        (st,_,_)  <- get
        let val = M.lookup l st
        case val of
            Nothing -> throwError $ show $ ErrorUninitializedVariable id pos
            Just v -> modifyMem $ \mem -> M.adjust incResult l mem
        transStmts xs


    G.Decr pos ident -> do
        env <- ask
        id <- transIdent ident
        let Just l = M.lookup id (varEnv env)
        (st,_,_)  <- get
        let val = M.lookup l st
        case val of
            Nothing -> throwError $ show $ ErrorUninitializedVariable id pos
            Just v -> modifyMem $ \mem -> M.adjust decResult l mem
        transStmts xs

    G.Ret _ expr -> transExpr expr
            
    G.VRet _ -> return MyVoid

    G.Cond _ expr block -> do
            MyBool e <- transExpr expr
            case e of
                True -> do
                    ret <- transBlock block
                    case ret of
                        MyBreak -> return MyBreak
                        MyContinue -> return MyContinue
                        _ -> transStmts xs 
                False -> transStmts xs

    G.CondElse _ expr block1 block2 -> do
            MyBool e <- transExpr expr
            case e of
                True -> do
                    ret <- transBlock block1
                    case ret of
                        MyBreak -> return MyBreak
                        MyContinue -> return MyContinue
                        _ -> transStmts xs 
                False -> do
                    ret <- transBlock block2
                    case ret of
                        MyBreak -> return MyBreak
                        MyContinue -> return MyContinue
                        _ -> transStmts xs 

    G.While pos expr block -> do
            MyBool w <- transExpr expr
            case w of
                False -> transStmts xs
                True -> do
                    ret <- transBlock block
                    case ret of
                        MyBreak -> transStmts xs
                        _ -> transStmts ((G.While pos expr block):xs)
                
    G.SExp _ expr -> transExpr expr >> transStmts xs

    G.Break _ -> return MyBreak

    G.Continue _ -> return MyContinue


transAppFunc :: G.FnDef -> [G.Expr] -> Env -> Result TypeOfResult
transAppFunc (G.FnDef _ type_ ident args block) expr env = do
    new_env <- doFunc args expr env
    local (\e -> new_env) (transBlock block)
                    
doFunc :: [G.Arg] -> [G.Expr] -> Env -> Result Env
doFunc [] [] env = return env
doFunc (arg:args) (expr:exprs) env = case arg of

    G.Arg _ type_ ident -> do
        e <- transExpr expr
        id <- transIdent ident
        l <- newloc
        modifyMem (M.insert l e)
        let new_env = env { varEnv = M.insert id l (varEnv env) }
        doFunc args exprs new_env

    G.ArgRef _ type_ ident -> do 
        new_id <- transIdent ident
        let G.EVar _ old_ident = expr
        old_id <- transIdent old_ident
        old_env <- ask
        let Just l = M.lookup old_id (varEnv old_env)
        let new_env =  env { varEnv = M.insert new_id l (varEnv env) }
        doFunc args exprs new_env


transExpr ::  G.Expr -> Result TypeOfResult
transExpr x = case x of

    G.EVar pos ident -> do
        env <- ask
        id <-  transIdent ident
        let Just l = M.lookup id (varEnv env)
        (st,_,_)  <- get
        let val = M.lookup l st
        case val of
            Nothing -> throwError $ show $ ErrorUninitializedVariable id pos
            Just v -> return v

    G.ELitInt _ integer -> return $ MyInt integer
    G.ELitTrue _ -> return $ MyBool True
    G.ELitFalse _ -> return $ MyBool False

    G.EApp _ ident exprs -> do
        e <- mapM transExpr exprs
        id <- transIdent ident
        case id of

            "print" -> do
                liftIO $ putStrLn (prettyPrint e)
                return $ MyBool True

            _ -> do
                (st,_,globFunc) <- get
                env <- ask
                let ml = M.lookup id (varEnv env)
                case ml of
                    Nothing -> do   -- funkcja globalna
                        let Just (fun, env) = M.lookup id globFunc
                        local (\e -> e) (transAppFunc fun exprs env)
                    Just l -> do
                        let Just (MyFunc func old_env) = M.lookup l st
                        local (\e -> e) (transAppFunc func exprs old_env)
                            
    G.EString _ string -> return $ MyStr string

    G.Neg _ expr -> do
        MyInt e <- transExpr expr
        return $ MyInt (-1 * e)

    G.Not _ expr -> do
        MyBool e <- transExpr expr
        return $ MyBool (not e)
  
    G.EMul pos expr1 mulop expr2 -> do
        MyInt e1 <- transExpr expr1
        MyInt e2 <- transExpr expr2
        (op, div) <- transMulOp mulop
        when (div && (e2 == 0)) $ throwError $ show $ ErrorDevisionByZero pos
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


interpret :: G.Program -> Result () 
interpret = transProgram

runInterpreter program = 
    let (newEnv, newState) = (Env { varEnv = M.empty }, (M.empty, 0, M.empty))
    in do 
    (res, a) <- runReaderT (runStateT (runExceptT (interpret program)) newState) newEnv
    return res

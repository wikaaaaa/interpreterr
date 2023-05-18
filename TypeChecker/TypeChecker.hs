{-# LANGUAGE FlexibleContexts #-}
module TypeChecker.TypeChecker where

import qualified AbsGramar as G
import qualified Data.Map as M
import Data.Maybe(Maybe(..))
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Set as Set
import TypeChecker.TypeCheckerEnv
import TypeChecker.TypeCheckerUtils

transProgram ::  G.Program -> Result Type
transProgram (G.Program _ topdefs) = do
    env <- ask
    env_with_func <- transTopDefsFuncOnly topdefs env
    local (\e -> env_with_func) (transTopDefs topdefs)

transBlockWithRet ::  G.Block -> String -> G.BNFC'Position -> Result Type
transBlockWithRet (G.Block _ stmts) name pos = do
    ret <- local (\e -> e) (transStmts stmts)
    case ret of
        MyNothing -> throwError $ show $ ErrorNoReturn name pos
        _ -> return ret

transBlockNoRet ::  G.Block -> String -> Result ()
transBlockNoRet (G.Block pos stmts) name = do
    res <- local (\e -> e) (transStmts stmts)
    case res of 
        MyNothing -> return ()
        _ -> throwError $ show $ ErrorReturn ("istn't allowed in " ++ name ++ " block") pos

transArg :: [G.Arg] -> [Type] -> Result [Type]
transArg [] res = return res
transArg (x:xs) res =
    case x of
        G.Arg _ type_ ident -> do
            t <- transTypeNotVoid type_
            transArg xs (res ++ [t]) 
        G.ArgRef _ type_ ident -> do
            t <- transTypeNotVoid type_
            let ref_t = MyRef t 
            transArg xs (res ++ [ref_t])

addArgToEnv :: [G.Arg] -> Env -> Result Env
addArgToEnv [] env = return env
addArgToEnv (x:xs) e = do

    case x of 
        G.Arg pos type_ ident -> do
            t <- transTypeNotVoid type_
            id <- transIdent ident
            checkIfAvailableVarInEnv id e pos
            let new_env = e { varType = M.insert id t (varType e), names = Set.insert id ( names e ) }
            addArgToEnv xs new_env

        G.ArgRef pos type_ ident -> do
            t <- transTypeNotVoid type_
            id <- transIdent ident  
            checkIfAvailableVarInEnv id e pos
            let new_env = e { varType = M.insert id t (varType e), names = Set.insert id ( names e ) }
            addArgToEnv xs new_env

checkArgs :: [Type] -> [G.Expr] -> String -> G.BNFC'Position -> Int -> Result ()
checkArgs [] [] name pos i = return ()
checkArgs a [] name pos i = throwError $ show $ ErrorTooFewArguments name pos 
checkArgs [] a name pos i = throwError $ show $ ErrorTooManyArguments name pos 

checkArgs (arg:args) (expr:exprs) name pos i = case arg of

    MyRef ref -> case expr of
        G.EVar _ _ -> do
                e <- transExpr expr
                when (ref /= e) $ throwError $ show $ ErrorArgumentTypeMismatch name i ref e pos 
                checkArgs args exprs name pos (i+1)
        _ -> throwError $ show $ ErrorReference name i pos

    a -> do
        e <- transExpr expr
        when (a /= e) $ throwError $ show $ ErrorArgumentTypeMismatch name i a e pos 
        checkArgs args exprs name pos (i+1)

transTopDefsFuncOnly :: [G.TopDef] -> Env -> Result Env
transTopDefsFuncOnly [] env = return env
transTopDefsFuncOnly (y:ys) env = case y of

    G.Fn _ (G.FnDef pos type_ ident args block) -> do
        id <- transIdent ident
        case id of
            "main" -> do
                case ys of
                    [] -> do
                        ret_type <- transType type_
                        when (ret_type /= MyVoid) $ throwError $ show $ ErrorMainWrongReturnType ret_type pos
                        when (args /= []) $ throwError $ show $ ErrorMainHasArguments pos
                        return env

                    _ -> do
                    throwError $ show $ ErrorMainNotLastDeclaration pos
                
            _ -> do
                checkIfAvailableFunc id env pos
                ret_type <- transType type_
                args_type <- transArg args []
                let res = MyFunc ret_type args_type                  
                let new_env  = env { varType = M.insert id res (varType env), names = Set.insert id ( names env )  }
                transTopDefsFuncOnly ys new_env

    G.VarDef _ type_ item -> transTopDefsFuncOnly ys env

transTopDefs :: [G.TopDef] -> Result Type
transTopDefs [] = return MyVoid
transTopDefs (y:ys) = case y of

    G.Fn _ (G.FnDef pos type_ ident args block) -> do
        id <- transIdent ident
        case id of
            "main" -> do
                ret_type <- transType type_
                env <- ask
                let new_env = env { names = Set.empty }
                returned_type <- local (\e -> new_env) (transBlockWithRet block id pos)
                when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
                return MyVoid
                  
            _ -> do
                env <- ask
                let new_env = env { names = Set.insert id Set.empty }
                new_env <- addArgToEnv args new_env
                ret_type <- transType type_
                returned_type <- local (\e -> new_env) (transBlockWithRet block id pos)
                when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
                local (\e -> e) (transTopDefs ys)

    G.VarDef pos type_ item -> case item of
        G.NoInit _ ident -> do
            id <- transIdent ident
            checkIfAvailableVar id pos
            t <- transTypeNotVoid type_
            local (\e -> e { varType = M.insert id t (varType e), names = Set.insert id ( names e ) } ) (transTopDefs ys)

        G.Init pos ident expr -> do
            e <- transExpr expr
            id <- transIdent ident
            checkIfAvailableVar id pos
            t <- transTypeNotVoid type_
            when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
            local (\e -> e { varType = M.insert id t (varType e) , names = Set.insert id ( names e ) }) (transTopDefs ys)


transStmts ::  [G.Stmt ] -> Result Type
transStmts [] = return MyNothing
transStmts (x:xs) = case x of

    G.Empty _ -> return MyNothing
    
    G.Decl _ topdef -> case topdef of
        
        G.Fn _ (G.FnDef pos type_ ident args block) -> do
            id <- transIdent ident
            env <- ask
            checkIfAvailableFunc id env pos
            ret_type <- transType type_
            args_type <- transArg args []

            let res = MyFunc ret_type args_type

            let env_with_func = env { varType = M.insert id res (varType env), names = Set.insert id ( Set.empty )  }

            new_env <- addArgToEnv args env_with_func
            
            returned_type <- local (\e -> new_env) (transBlockWithRet block id pos)
            when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
            
            local (\e -> e { varType = M.insert id res (varType e), names = Set.insert id ( names e )  }) (transStmts xs)

        G.VarDef pos type_ item -> case item of
            G.NoInit _ ident -> do
                id <- transIdent ident
                checkIfAvailableVar id pos
                t <- transTypeNotVoid type_
                local (\e -> e { varType = M.insert id t (varType e), names = Set.insert id ( names e )  }) (transStmts xs)


            G.Init pos ident expr -> do
                e <- transExpr expr
                id <- transIdent ident
                checkIfAvailableVar id pos
                t <- transTypeNotVoid type_
                when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
                local (\e -> e { varType = M.insert id t (varType e) , names = Set.insert id ( names e ) }) (transStmts xs)

    G.Ass pos ident expr -> do
        t <- transExpr expr
        id <- transIdent ident
        env <- ask
        let i = M.lookup id (varType env)
        case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just tt -> do
                when (tt /= t) $ throwError $ show $ ErrorTypeMismatch tt t pos
                transStmts xs

    G.Incr pos ident -> do
        id <- transIdent ident
        env <- ask
        let i = M.lookup id (varType env)
        case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just tt -> do
                        when (tt /= MyInt) $ throwError $ show $ ErrorTypeMismatch MyInt tt pos
                        transStmts xs                     

    G.Decr pos ident -> do
        id <- transIdent ident
        env <- ask
        let i = M.lookup id (varType env)
        case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just tt -> do
                        when (tt /= MyInt) $ throwError $ show $ ErrorTypeMismatch MyInt tt pos
                        transStmts xs

    G.Ret pos expr -> do 
        e <- transExpr expr
        case xs of
            [] -> return e
            _ -> throwError $ show $ ErrorReturn " istn't last statement in block, return statement" pos
                
    G.VRet pos -> do
        case xs of
            [] -> return MyVoid
            _ -> throwError $ show $ ErrorReturn " istn't last statement in block, return statement" pos

    G.Cond pos expr block -> do
        e <- transExpr expr
        when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
        local (\e -> e {names = Set.empty}) (transBlockNoRet block "'if'")
        transStmts xs
            
    G.CondElse pos expr block1 block2 -> do
        e <- transExpr expr
        when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
        local (\e -> e {names = Set.empty}) (transBlockNoRet block2 "'else'")
        local (\e -> e {names = Set.empty}) (transBlockNoRet block1 "'if'")
        transStmts xs

    G.While pos expr block -> do
        e <- transExpr expr
        when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
        local (\e -> e { inWhile = True, names = Set.empty }) (transBlockNoRet block "'while'")
        transStmts xs

    G.SExp _ expr -> transExpr expr >> transStmts xs

    G.Break pos -> do
        env <- ask
        let i = inWhile env
        case i of
            True -> transStmts xs
            False -> throwError $ show $ ErrorWhile "break" pos
                
    G.Continue pos -> do
        env <- ask
        let i = inWhile env
        case i of
            True -> transStmts xs
            False -> throwError $ show $ ErrorWhile "continue" pos


transExpr ::  G.Expr -> Result Type
transExpr x = case x of

    G.EVar pos ident -> do
        id <- transIdent ident
        env <- ask
        let i = M.lookup id (varType env)
        case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just t -> return t 

    G.ELitInt _ integer -> return MyInt
    G.ELitTrue _ -> return MyBool
    G.ELitFalse _ -> return MyBool

    G.EApp pos ident exprs -> do  

        e <- mapM transExpr exprs
        id <- transIdent ident
        case id of
            "print" -> do
                checkPrintArgs e pos 1
                return $ MyVoid
            _ -> do
                env <- ask
                let i = M.lookup id (varType env)
                case i of
                    Nothing -> throwError $ show $ ErrorUndefinedFunction id pos
                    Just val -> case val of
                        MyFunc ret args -> do
                            checkArgs args exprs id pos 1
                            return ret
                        _ -> throwError $ show $ ErrorNotFunction id pos

    G.EString pos string -> return MyStr

    G.Neg pos expr -> do
        e <- transExpr expr
        ensureMyType pos e MyInt
        return MyInt

    G.Not pos expr -> do
        e <- transExpr expr
        ensureMyType pos e MyBool
        return MyBool
    
    G.EMul pos expr1 mulop expr2 -> do
        e1 <- transExpr expr1
        e2 <- transExpr expr2
        ensureMyType pos e1 MyInt
        ensureMyType pos e2 MyInt
        return MyInt

    G.EAdd pos expr1 addop expr2 -> do
        e1 <- transExpr expr1
        e2 <- transExpr expr2
        ensureMyType pos e1 MyInt
        ensureMyType pos e2 MyInt
        return MyInt

    G.ERel pos expr1 relop expr2 -> do
        e1 <- transExpr expr1
        e2 <- transExpr expr2
        ensureMyType pos e1 MyInt
        ensureMyType pos e2 MyInt
        return MyBool

    G.EAnd pos expr1 expr2 -> do
        e1 <- transExpr expr1
        e2 <- transExpr expr2
        ensureMyType pos e1 MyBool
        ensureMyType pos e2 MyBool
        return MyBool

    G.EOr pos expr1 expr2 ->  do
        e1 <- transExpr expr1
        e2 <- transExpr expr2
        ensureMyType pos e1 MyBool
        ensureMyType pos e2 MyBool
        return MyBool

runEnvR r = runReader r Env { varType = M.empty, inWhile = False, names = Set.empty} 

runResult :: Result a -> Either String a
runResult t = runEnvR $ runExceptT t

typechecker :: G.Program -> Result Type
typechecker exp = catchError (transProgram exp) handler where
    handler e = throwError $ e

runTypeChecker :: G.Program -> Either String Type    
runTypeChecker exp = runResult (typechecker exp)

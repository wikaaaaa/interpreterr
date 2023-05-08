{-# LANGUAGE FlexibleContexts #-}
module TypeChecker where

import Prelude
  ( ($), (.) , Eq
  , Bool(..), (==), (<), (<=), (>), (>=), (/=), not, (&&), (||)
  , Int, Integer, Double, (+), (-), (*), div, mod
  , String, (++), print
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span, Show,  Either(..), IO, show, error, undefined, putStrLn, fst, putStr
  )
import qualified AbsGramar as G
import qualified Data.Map as M
import Data.Maybe(fromMaybe, Maybe(..))
import Control.Monad.Reader

import Control.Monad.Except


data Type = MyInt | MyBool | MyStr | MyVoid | MyFunc Type [Type]
    deriving(Eq)

instance Show Type where
  show (MyInt) = "int"
  show (MyBool) = "bool"
  show (MyStr) = "string"
  show (MyVoid) = "void"
  show (MyFunc t1 t2) = "function (" ++ show t2 ++ show t1 ++ ")"

data Env = Env { 
    varType :: M.Map String Type, 
    retType :: Maybe Type,
    ret :: Bool
}

type Result a = ExceptT String (Reader Env) a

data MyError = ErrorTypeMismatch Type Type G.BNFC'Position 
             | ErrorUndefinedVariable String G.BNFC'Position
             | ErrorNoReturn String G.BNFC'Position
             | ErrorReturnTypeMismatch String Type Type G.BNFC'Position
             | ErrorUndefinedFunction String G.BNFC'Position
             | ErrorArgumentTypeMismatch String Int Type Type G.BNFC'Position
             | ErrorTooFewArguments String G.BNFC'Position
             | ErrorTooManyArguments String G.BNFC'Position
             | ErrorMainNotLastDeclaration G.BNFC'Position
             | ErrorMainWrongReturnType Type G.BNFC'Position


printPos Nothing = ""
printPos (Just (l,c)) = " at line " ++ show l ++ ", column " ++ show c

instance Show MyError where
  show (ErrorTypeMismatch expected actual pos) = "TypesMismatchError \n expected type: " ++ show expected ++ ", actual type: " ++ show actual ++ printPos pos
  show (ErrorUndefinedVariable name pos) = "UndefinedVariableError\n undefined variable " ++ name ++  printPos pos
  show (ErrorNoReturn name pos) = "NoReturnError\n Funtion " ++ name ++ " declared "  ++ printPos pos ++ " has no return statement"
  show (ErrorReturnTypeMismatch name expected actual pos) = "ReturnTypeMismatchError \n Wrong type of the returned value in function " ++ name ++ " declared "++ printPos pos
                                                             ++ "\n expected type: " ++ show expected ++ ", actual type: " ++ show actual 
  show (ErrorUndefinedFunction name pos) = "UndefinedFunctionError\n undefined function " ++ name ++  printPos pos
  show (ErrorArgumentTypeMismatch name numb expected actual pos) = "ArgumentTypeMismatchError \n Wrong type of argument number " ++ show numb ++  " in application of function " ++ name ++ printPos pos
                                                             ++ "\n expected type: " ++ show expected ++ ", actual type: " ++ show actual 
  show (ErrorTooFewArguments name pos) = "TooFewArgumentsError\n too few arguments in application of function " ++ name ++ printPos pos
  show (ErrorTooManyArguments name pos) = "TooManyArgumentsError\n too many arguments in application of function " ++ name ++ printPos pos
  show (ErrorMainNotLastDeclaration pos) = "MainNotLastDeclarationError \n" ++ "Function main (declared " ++ printPos pos ++ ") istn't the last definition in the program" 
  show (ErrorMainWrongReturnType t pos) = "MainWrongReturnTypeError \n" ++ "Wrong return type at main declaration " ++ printPos pos 
                                          ++ "\n expected type: void, actual type: " ++ show t

transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transProgram ::  G.Program -> Result Type
transProgram x = case x of
  G.Program _ topdefs -> transTopDefs topdefs


transBlockWithRet ::  G.Block -> String -> G.BNFC'Position -> Result Type
transBlockWithRet x name pos = case x of
  G.Block _ stmts -> do
    ret <- transStmts stmts
    case ret of
      Nothing -> throwError $ show $ ErrorNoReturn name pos
      Just r -> return r

transBlock ::  G.Block -> Result Type
transBlock x = case x of
  G.Block _ stmts -> transStmts stmts >> return MyVoid

transArg :: [G.Arg] -> [Type] -> Result [Type]
transArg [] res = return res
transArg (x:xs) res = do
    case x of
      G.Arg _ type_ ident -> do
        t <- transType type_
        transArg xs (res ++ [t]) 
      G.ArgRef _ type_ ident -> do
        t <- transType type_
        transArg xs (res ++ [t])


addArgToEnv :: [G.Arg] -> Env -> Result Env
addArgToEnv [] env = return env
addArgToEnv (x:xs) e = do
      case x of 
        G.Arg _ type_ ident -> do
              t <- transType type_
              id <- transIdent ident
              let new_env = e { varType = M.insert id t (varType e) }
              addArgToEnv xs new_env
        G.ArgRef _ type_ ident -> do
              t <- transType type_
              id <- transIdent ident  
              let new_env = e { varType = M.insert id t (varType e) }
              addArgToEnv xs new_env

transTopDefs :: [G.TopDef] -> Result Type
transTopDefs [] = return MyVoid
transTopDefs (y:ys) = case y of

    G.Fn _ (G.FnDef pos type_ ident args block) -> do
              id <- transIdent ident
              case id of
                "main" -> do
                  case ys of
                    [] -> do
                      ret_type <- transType type_
                      when (ret_type /= MyVoid) $ throwError $ show $ ErrorMainWrongReturnType ret_type pos
                      returned_type <- transBlockWithRet block id pos
                      when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
                      return MyVoid

                    _ -> do
                      throwError $ show $ ErrorMainNotLastDeclaration pos
                  
                _ -> do
                  env <- ask
                  new_env <- addArgToEnv args env
                  ret_type <- transType type_
                  args_type <- transArg args []
                  let res = MyFunc ret_type args_type
                  let new_env_ = new_env { varType = M.insert id res (varType new_env) }

                  returned_type <- local (\e -> new_env_) (transBlockWithRet block id pos)
                  when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
                  
                  local (\e -> e { varType = M.insert id res (varType e) }) (transTopDefs ys)

     
    G.VarDef _ type_ item -> case item of
      G.NoInit _ ident -> do
        id <- transIdent ident
        t <- transType type_
        local (\e -> e { varType = M.insert id t (varType e) }) (transTopDefs ys)


      G.Init pos ident expr -> do
        e <- transExpr expr
        id <- transIdent ident
        t <- transType type_
        when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
        local (\e -> e { varType = M.insert id t (varType e) }) (transTopDefs ys)


transStmts ::  [G.Stmt ] -> Result (Maybe Type)
transStmts [] = return Nothing
transStmts (x:xs) = case x of

  G.Empty _ -> return Nothing

  G.BStmt _ block -> undefined
  
  G.Decl _ topdef -> case topdef of
    
      G.Fn _ fndef -> undefined

      G.VarDef _ type_ item -> case item of
          G.NoInit _ ident -> do
            id <- transIdent ident
            t <- transType type_
            local (\e -> e { varType = M.insert id t (varType e) }) (transStmts xs)


          G.Init pos ident expr -> do
            e <- transExpr expr
            id <- transIdent ident
            t <- transType type_
            when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
            local (\e -> e { varType = M.insert id t (varType e) }) (transStmts xs)

  -- zakładając ze zmienna byla wczesniej zadeklarowana
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

  G.Ret _ expr -> do 
            e <- transExpr expr
            -- sprawdzic czy return jest ostatnia operacja w bloku?
            -- sprawdzenie czy zwracany tym sie zgadza eh
            return $ Just e
          
  G.VRet _ -> return $ Just (MyVoid)

  G.Cond pos expr block -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          transBlock block
          transStmts xs
          
  G.CondElse pos expr block1 block2 -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          transBlock block2
          transBlock block1
          transStmts xs

  G.While pos expr block -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          transBlock block
          transStmts xs

  G.SExp _ expr -> transExpr expr >> transStmts xs -- aplikacja funkcji

  G.Break _ -> undefined
  G.Continue _ -> undefined

ensureMyInt pos expr = do
        t <- transExpr expr
        when (t /= MyInt) $ throwError $ show $ ErrorTypeMismatch MyInt t pos

ensureMyBool pos expr = do
        t <- transExpr expr
        when (t /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool t pos

ensureMyStr pos expr = do
        t <- transExpr expr
        when (t /= MyStr) $ throwError $ show $ ErrorTypeMismatch MyStr t pos

checkArgs :: [Type] -> [Type] -> String -> G.BNFC'Position -> Int -> Result ()
checkArgs [] [] name pos i = return ()
checkArgs a [] name pos i = throwError $ show $ ErrorTooFewArguments name pos 
checkArgs [] a name pos i = throwError $ show $ ErrorTooManyArguments name pos 
checkArgs (arg:args) (expr:exprs) name pos i = do
        when (arg /= expr) $ throwError $ show $ ErrorArgumentTypeMismatch name i arg expr pos 
        checkArgs args exprs name pos (i+1)

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
                  "print" -> return $ MyVoid
                  _ -> do
                    env <- ask
                    let i = M.lookup id (varType env)
                    case i of
                        Nothing -> throwError $ show $ ErrorUndefinedFunction id pos
                        Just val -> case val of
                            MyFunc ret args -> do
                                checkArgs args e id pos 1
                                return ret
                            _ -> undefined

  G.EString pos string -> return MyStr

  G.Neg pos expr -> do
          ensureMyInt pos expr
          return MyInt

  G.Not pos expr -> do
          ensureMyBool pos expr
          return MyBool
  
  G.EMul pos expr1 mulop expr2 -> do
          ensureMyInt pos expr1
          ensureMyInt pos expr2
          return MyInt

  G.EAdd pos expr1 addop expr2 -> do
          ensureMyInt pos expr1
          ensureMyInt pos expr2
          return MyInt

  G.ERel pos expr1 relop expr2 -> do
          ensureMyInt pos expr1
          ensureMyInt pos expr2
          return MyBool

  G.EAnd pos expr1 expr2 -> do
          ensureMyBool pos expr1
          ensureMyBool pos expr2
          return MyBool

  G.EOr pos expr1 expr2 ->  do
          ensureMyBool pos expr1
          ensureMyBool pos expr2
          return MyBool


transType ::  G.Type -> Result Type
transType x = case x of
  G.MyInt _ -> return MyInt
  G.MyStr _ -> return MyStr
  G.MyBool _ -> return MyBool
  G.MyVoid _ -> return MyVoid



runEnvR r = runReader r Env { varType = M.empty, retType = Nothing, ret = False } 

runResult :: Result a -> Either String a
runResult t = runEnvR $ runExceptT t

typechecker :: G.Program -> Result Type
typechecker exp = catchError (transProgram exp) handler where
  handler e = throwError $ e

runTypeChecker :: G.Program -> Either String Type    
runTypeChecker exp = runResult (typechecker exp)


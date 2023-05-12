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
import qualified Data.Set as Set


data Type = MyInt | MyBool | MyStr | MyVoid | MyFunc Type [Type] | MyRef Type | MyNothing
    deriving(Eq)

instance Show Type where
  show (MyInt) = "int"
  show (MyBool) = "bool"
  show (MyStr) = "string"
  show (MyVoid) = "void"
  show (MyFunc t1 t2) = "function (" ++ show t2 ++ show t1 ++ ")"
  show (MyRef t) = "ref" ++ show t

data Env = Env { 
    varType :: M.Map String Type, 
    retType :: Maybe Type,
    ret :: Bool,
    inWhile :: Bool,
    names :: Set.Set String
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
             | ErrorMainHasArguments G.BNFC'Position
             | ErrorPrint Int G.BNFC'Position
             | ErrorReference String Int G.BNFC'Position
             | ErrorUsedName String String G.BNFC'Position
             | ErrorWhile String G.BNFC'Position
             | ErrorReturn String G.BNFC'Position


printPos Nothing = ""
printPos (Just (l,c)) = " at line " ++ show l ++ ", column " ++ show c

instance Show MyError where
  show (ErrorTypeMismatch expected actual pos) = "TypesMismatchError \n expected type: " ++ show expected ++ ", actual type: " ++ show actual ++ printPos pos
  show (ErrorUndefinedVariable name pos) = "UndefinedVariableError\n undefined variable " ++ name ++  printPos pos
  show (ErrorNoReturn name pos) = "NoReturnError\n Funtion " ++ name ++ " declared "  ++ printPos pos ++ " has no return statement"
  show (ErrorReturnTypeMismatch name expected actual pos) = "ReturnTypeMismatchError \n Wrong type of the returned value in function " ++ name ++ " declared" ++ printPos pos
                                                             ++ "\n expected type: " ++ show expected ++ ", actual type: " ++ show actual 
  show (ErrorUndefinedFunction name pos) = "UndefinedFunctionError\n undefined function " ++ name ++  printPos pos
  show (ErrorArgumentTypeMismatch name numb expected actual pos) = "ArgumentTypeMismatchError \n Wrong type of argument number " ++ show numb ++  " in application of function " ++ name ++ printPos pos
                                                             ++ "\n expected type: " ++ show expected ++ ", actual type: " ++ show actual 
  show (ErrorTooFewArguments name pos) = "TooFewArgumentsError\n too few arguments in application of function " ++ name ++ printPos pos
  show (ErrorTooManyArguments name pos) = "TooManyArgumentsError\n too many arguments in application of function " ++ name ++ printPos pos
  show (ErrorMainNotLastDeclaration pos) = "MainNotLastDeclarationError \n" ++ "Function main (declared" ++ printPos pos ++ ") istn't the last definition in the program" 
  show (ErrorMainWrongReturnType t pos) = "MainWrongReturnTypeError \n" ++ "Wrong return type at main declaration" ++ printPos pos 
                                          ++ "\n expected type: void, actual type: " ++ show t
  show (ErrorMainHasArguments pos) = "MainHasArgumentsError \n Error in main declared" ++ printPos pos ++ ", function main doesn't take arguments"
  show (ErrorPrint nb pos) = "PrintError \n Error in usage of print" ++ printPos pos ++ "\nType of argument number " ++ show nb ++ " is void, print argument cannot be void"
  show (ErrorReference name nb pos) = "ReferenceError \n Error in use of function " ++ name ++ printPos pos ++ ", argument number " ++ show nb ++ " is not a variable, \n argument passed by reference must be a variable"
  show (ErrorUsedName what name pos) = "UsedNameError\n Error in " ++ what ++ " declaration" ++ printPos pos ++ "\n Name " ++ name ++ " is already in use"
  show (ErrorWhile name pos) = "WhileError \n incorrect use of " ++ name  ++ printPos pos ++ " - " ++ name ++ " used not in while loop"
  show (ErrorReturn err_type pos) = "ReturnError\n " ++ "Return statement " ++ err_type ++ printPos pos


transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transProgram ::  G.Program -> Result Type
transProgram x = case x of
  G.Program _ topdefs -> do
    env <- ask
    env_with_func <- transTopDefsFuncOnly topdefs env
    local (\e -> env_with_func) (transTopDefs topdefs)


transBlockWithRet ::  G.Block -> String -> G.BNFC'Position -> Result Type
transBlockWithRet x name pos = case x of
  G.Block _ stmts -> do
    ret <- local (\e -> e { names = Set.empty }) (transStmts stmts)
    case ret of
      MyNothing -> throwError $ show $ ErrorNoReturn name pos
      _ -> return ret

-- block with no result statement
transBlock ::  G.Block -> String -> Result ()
transBlock (G.Block pos stmts) name = do
          res <- local (\e -> e { names = Set.empty }) (transStmts stmts)
          case res of 
              MyNothing -> return ()
              _ -> throwError $ show $ ErrorReturn ("istn't allowed in " ++ name ++ " block") pos

    

transArg :: [G.Arg] -> [Type] -> Result [Type]
transArg [] res = return res
transArg (x:xs) res = do
    case x of
      G.Arg _ type_ ident -> do
        t <- transType type_
        transArg xs (res ++ [t]) 
      G.ArgRef _ type_ ident -> do
        t <- transType type_
        let ref_t = MyRef t 
        transArg xs (res ++ [ref_t])


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
                      returned_type <- transBlockWithRet block id pos
                      when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
                      return MyVoid
                  
                _ -> do
                  env <- ask
                  new_env <- addArgToEnv args env
                  ret_type <- transType type_
                  returned_type <- local (\e -> new_env) (transBlockWithRet block id pos)
                  when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
                  local (\e -> e) (transTopDefs ys) -- moze ten local nie jest niebedny

     
    G.VarDef pos type_ item -> case item of
      G.NoInit _ ident -> do
        id <- transIdent ident
        checkIfAvailableVar id pos
        t <- transType type_
        local (\e -> e { varType = M.insert id t (varType e), names = Set.insert id ( names e ) } ) (transTopDefs ys)


      G.Init pos ident expr -> do
        e <- transExpr expr
        id <- transIdent ident
        checkIfAvailableVar id pos
        t <- transType type_
        when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
        local (\e -> e { varType = M.insert id t (varType e) , names = Set.insert id ( names e ) }) (transTopDefs ys)


checkIfAvailableFunc :: String -> Env -> G.BNFC'Position -> Result ()
checkIfAvailableFunc id env pos = do
          when (id == "print") $ throwError $ show $ ErrorUsedName "function" id pos
          when (id == "main") $ throwError $ show $ ErrorUsedName "function" id pos
          let i = Set.member id (names env)
          case i of
            False -> return ()
            True -> throwError $ show $ ErrorUsedName "function" id pos

checkIfAvailableVar :: String -> G.BNFC'Position -> Result ()
checkIfAvailableVar id pos = do
          env <- ask
          let i = Set.member id (names env)
          case i of
            False -> return ()
            True  -> throwError $ show $ ErrorUsedName "variable" id pos


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
            new_env <- addArgToEnv args env
            args_type <- transArg args []
            returned_type <- local (\e -> new_env) (transBlockWithRet block id pos)
            when (returned_type /= ret_type) $ throwError $ show $ ErrorReturnTypeMismatch id ret_type returned_type pos
            let res = MyFunc ret_type args_type 
            local (\e -> e { varType = M.insert id res (varType e), names = Set.insert id ( names e )  }) (transStmts xs)

      G.VarDef pos type_ item -> case item of
          G.NoInit _ ident -> do
            id <- transIdent ident
            checkIfAvailableVar id pos
            t <- transType type_
            local (\e -> e { varType = M.insert id t (varType e), names = Set.insert id ( names e )  }) (transStmts xs)


          G.Init pos ident expr -> do
            e <- transExpr expr
            id <- transIdent ident
            checkIfAvailableVar id pos
            t <- transType type_
            when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
            local (\e -> e { varType = M.insert id t (varType e) , names = Set.insert id ( names e ) }) (transStmts xs)

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

  G.Ret pos expr -> do 
            e <- transExpr expr
            -- sprawdzic czy return jest ostatnia operacja w bloku?
            -- sprawdzenie czy zwracany tym sie zgadza eh
            case xs of
              [] -> return e
              _ -> throwError $ show $ ErrorReturn " istn't last statement in block, return statement" pos
            
          
  G.VRet pos -> do
          case xs of
            [] -> return MyVoid
            _ -> throwError $ show $ ErrorReturn " istn't last statement in block" pos
         

  G.Cond pos expr block -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          transBlock block "'if'"
          transStmts xs
          
  G.CondElse pos expr block1 block2 -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          transBlock block2 "'else'"
          transBlock block1 "'if'"
          transStmts xs

  G.While pos expr block -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          local (\e -> e { inWhile = True }) (transBlock block "'while'")
          transStmts xs

  G.SExp _ expr -> transExpr expr >> transStmts xs -- aplikacja funkcji

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

ensureMyInt pos expr = do
        t <- transExpr expr
        when (t /= MyInt) $ throwError $ show $ ErrorTypeMismatch MyInt t pos

ensureMyBool pos expr = do
        t <- transExpr expr
        when (t /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool t pos

ensureMyStr pos expr = do
        t <- transExpr expr
        when (t /= MyStr) $ throwError $ show $ ErrorTypeMismatch MyStr t pos

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

checkPrintArgs [] pos nb = return True
checkPrintArgs (expr:exprs) pos nb = do
        when (expr == MyVoid) $ throwError $ show $ ErrorPrint nb pos
        checkPrintArgs exprs pos (nb+1)

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


runEnvR r = runReader r Env { varType = M.empty, retType = Nothing, ret = False, inWhile = False, names = Set.empty} 

runResult :: Result a -> Either String a
runResult t = runEnvR $ runExceptT t

typechecker :: G.Program -> Result Type
typechecker exp = catchError (transProgram exp) handler where
  handler e = throwError $ e

runTypeChecker :: G.Program -> Either String Type    
runTypeChecker exp = runResult (typechecker exp)


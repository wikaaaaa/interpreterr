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


data Type = MyInt | MyBool | MyStr | MyVoid
    deriving(Eq)

instance Show Type where
  show (MyInt) = "int"
  show (MyBool) = "bool"
  show (MyStr) = "string"
  show (MyVoid) = "void"

data Env = Env { 
    varType :: M.Map String (Type, Bool), -- bool = czy zainicjalizowana
    funcType ::  M.Map String (G.FnDef, Env),
    retType :: Maybe Type,
    ret :: Bool
}

type Result a = ExceptT String (Reader Env) a

data MyError = ErrorTypeMismatch Type Type G.BNFC'Position 
             | ErrorUndefinedVariable String G.BNFC'Position
             | ErrorUninitializedVariable String G.BNFC'Position

printPos Nothing = ""
printPos (Just (l,c)) = " at line " ++ show l ++ ", column " ++ show c

instance Show MyError where
  show (ErrorTypeMismatch expected actual pos) = "TypesMismatchError \n expected type: " ++ show expected ++ ", actual type: " ++ show actual ++ printPos pos
  show (ErrorUndefinedVariable name pos) = "UndefinedVariableError\n undefined variable " ++ name ++  printPos pos
  show (ErrorUninitializedVariable name pos) = "UninitializedError\n unitialized variable " ++ name ++  printPos pos


transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transProgram ::  G.Program -> Result Type
transProgram x = case x of
  G.Program _ topdefs -> transTopDefs topdefs

transBlock ::  G.Block -> Result Type
transBlock x = case x of
  G.Block _ stmts -> transStmts stmts >> return MyVoid

transTopDefs :: [G.TopDef] -> Result Type
transTopDefs [] = return MyVoid
transTopDefs (y:ys) = case y of

    G.Fn _ (G.FnDef pos type_ ident args block) -> do
              id <- transIdent ident
              case id of
                "main" -> transBlock block >> return MyVoid
                _ -> do
                  env <- ask
                  let f = (G.FnDef pos type_ ident args block)
                  local (\e -> e { funcType = M.insert id (f, env) (funcType e) }) (transTopDefs ys)

     
    G.VarDef _ type_ item -> case item of
      G.NoInit _ ident -> do
        id <- transIdent ident
        t <- transType type_
        let i = (t, False)
        local (\e -> e { varType = M.insert id i (varType e) }) (transTopDefs ys)


      G.Init pos ident expr -> do
        e <- transExpr expr
        id <- transIdent ident
        t <- transType type_
        let i = (t, True)
        when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
        local (\e -> e { varType = M.insert id i (varType e) }) (transTopDefs ys)


transStmts ::  [G.Stmt ] -> Result (Maybe Type)
transStmts [] = return Nothing
transStmts (x:xs) = case x of

  G.Empty _ -> return Nothing

  G.BStmt _ block -> return Nothing
  
  G.Decl _ topdef -> case topdef of
    
      G.Fn _ fndef -> undefined

      G.VarDef _ type_ item -> case item of
          G.NoInit _ ident -> do
            id <- transIdent ident
            t <- transType type_
            let i = (t, False)
            local (\e -> e { varType = M.insert id i (varType e) }) (transStmts xs)


          G.Init pos ident expr -> do
            e <- transExpr expr
            id <- transIdent ident
            t <- transType type_
            let i = (t, True)
            when (e /= t) $ throwError $ show $ ErrorTypeMismatch t e pos
            local (\e -> e { varType = M.insert id i (varType e) }) (transStmts xs)

  -- zakładając ze zmienna byla wczesniej zadeklarowana
  G.Ass pos ident expr -> do
          t <- transExpr expr
          id <- transIdent ident
          env <- ask
          let i = M.lookup id (varType env)
          case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just (tt, initialized) -> do
                when (tt /= t) $ throwError $ show $ ErrorTypeMismatch tt t pos
                local (\e -> e { varType = M.insert id (t, True) (varType e) }) (transStmts xs)
              

  G.Incr pos ident -> do
          id <- transIdent ident
          env <- ask
          let i = M.lookup id (varType env)
          case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just (tt, initialized) -> case initialized of
                            False -> throwError $ show $ ErrorUninitializedVariable id pos 
                            True -> do
                              when (tt /= MyInt) $ throwError $ show $ ErrorTypeMismatch MyInt tt pos
                              transStmts xs
                                

  G.Decr pos ident -> do
          id <- transIdent ident
          env <- ask
          let i = M.lookup id (varType env)
          case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just (tt, initialized) -> case initialized of
                            False -> throwError $ show $ ErrorUninitializedVariable id pos 
                            True -> do
                              when (tt /= MyInt) $ throwError $ show $ ErrorTypeMismatch MyInt tt pos
                              transStmts xs

  G.Ret _ expr -> do 
            e <- transExpr expr
            -- sprawdzenie czy zwracany tym sie zgadza eh
            return $ Just e
          
  G.VRet _ -> return $ Just (MyVoid)

  G.Cond pos expr block -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          local(\env->env)(transBlock block)
          transStmts xs
          
  G.CondElse pos expr block1 block2 -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          local(\env->env)(transBlock block2) 
          local(\env->env)(transBlock block1)
          transStmts xs

  G.While pos expr block -> do
          e <- transExpr expr
          when (e /= MyBool) $ throwError $ show $ ErrorTypeMismatch MyBool e pos
          local(\env->env)(transBlock block)
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

transExpr ::  G.Expr -> Result Type
transExpr x = case x of
  G.EVar pos ident -> do
          id <- transIdent ident
          env <- ask
          let i = M.lookup id (varType env)
          case i of
            Nothing -> throwError $ show $ ErrorUndefinedVariable id pos
            Just (t, initialized) -> case initialized of
                            False -> throwError $ show $ ErrorUninitializedVariable id pos 
                            True -> return t 
          

  G.ELitInt _ integer -> return MyInt
  G.ELitTrue _ -> return MyBool
  G.ELitFalse _ -> return MyBool

  G.EApp _pos ident exprs -> undefined

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



runEnvR r = runReader r Env { varType = M.empty, funcType = M.empty, retType = Nothing, ret = False } 

runResult :: Result a -> Either String a
runResult t = runEnvR $ runExceptT t

typechecker :: G.Program -> Result Type
typechecker exp = catchError (transProgram exp) handler where
  handler e = throwError $ e

runTypeChecker :: G.Program -> Either String Type    
runTypeChecker exp = runResult (typechecker exp)


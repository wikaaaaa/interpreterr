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



data Env = Env { 
    varType :: M.Map String (Type, Bool), -- bool = czy zainicjalizowana
    retType :: Maybe Type,
    ret :: Bool
}

type Result a = ExceptT String (Reader Env) a


transIdent :: G.Ident -> Result String
transIdent x = case x of
  G.Ident string -> return string

transProgram ::  G.Program -> Result Type
transProgram x = case x of
  G.Program _ topdefs -> transTopDefs topdefs


transTopDefs :: [G.TopDef] -> Result Type
transTopDefs [] = return MyVoid
transTopDefs (y:ys) = case y of

    G.Fn _ fndef -> undefined--transFnDef fndef >> transTopDefs ys

     
    G.VarDef _ type_ item -> case item of
      G.NoInit _ ident -> do
        id <- transIdent ident
        t <- transType type_
        let i = (t, False)
        local (\e -> e { varType = M.insert id i (varType e) }) (transTopDefs ys)


      G.Init _ ident expr -> do
        e <- transExpr expr
        id <- transIdent ident
        t <- transType type_
        let i = (t, True)
        when (e /= t) $ throwError $ "error"
        local (\e -> e { varType = M.insert id i (varType e) }) (transTopDefs ys)


transExpr ::  G.Expr -> Result Type
transExpr x = case x of
  G.EVar _ ident -> undefined
              

  G.ELitInt _ integer -> return MyInt
  G.ELitTrue _ -> return MyBool
  G.ELitFalse _ -> return MyBool

  G.EApp _ ident exprs -> undefined

                    
  G.EString _ string -> return MyStr

  G.Neg _ expr -> undefined
  G.Not _ expr -> undefined
  
  G.EMul _ expr1 mulop expr2 ->undefined

  G.EAdd _ expr1 addop expr2 -> undefined

  G.ERel _ expr1 relop expr2 ->  undefined

  G.EAnd _ expr1 expr2 ->undefined

  G.EOr _ expr1 expr2 -> undefined


transType ::  G.Type -> Result Type
transType x = case x of
  G.MyInt _ -> return MyInt
  G.MyStr _ -> return MyStr
  G.MyBool _ -> return MyBool
  G.MyVoid _ -> return MyVoid



runEnvR r = runReader r Env { varType = M.empty, retType = Nothing, ret = False } 

runTCM :: Result a -> Either String a
runTCM t = runEnvR $ runExceptT t

--runExceptT :: TCM a -> Reader Env (Either String a)

typeOf1 :: G.Program -> Result Type
typeOf1 exp = catchError (transProgram exp) handler where
  handler e = throwError $ 
    "Type error in\n"
    ++show exp++"\n"
    ++e

runTypeChecker :: G.Program -> Either String Type    
runTypeChecker exp = runTCM (typeOf1 exp)


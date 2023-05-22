{-# LANGUAGE FlexibleContexts #-}
module TypeChecker.TypeCheckerUtils where

import TypeChecker.TypeCheckerEnv
import qualified AbsGramar as G
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Set as Set

instance Show Type where
    show (MyInt) = "int"
    show (MyBool) = "bool"
    show (MyStr) = "string"
    show (MyVoid) = "void"
    show (MyFunc t1 t2) = "function (" ++ show t2 ++ show t1 ++ ")"
    show (MyRef t) = "ref" ++ show t

printPos Nothing = ""
printPos (Just (l,c)) = " at line " ++ show l ++ ", column " ++ show c

data MyError = ErrorTypeMismatch Type Type G.BNFC'Position 
             | ErrorTypeMismatch1 String Type G.BNFC'Position 
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
             | ErrorNotFunction String G.BNFC'Position
             | ErrorVoid G.BNFC'Position

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
    show (ErrorNotFunction name pos) = "NotFunctionError \n" ++ name ++ " used" ++ printPos pos ++ " is a variable not a function"
    show (ErrorVoid pos) = "VoidError\n type void not allowed" ++ printPos pos
    show (ErrorTypeMismatch1 expected actual pos) = "TypesMismatchError \n expected type: " ++ expected ++ ", actual type: " ++ show actual ++ printPos pos

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


checkIfAvailableVarInEnv :: String -> Env -> G.BNFC'Position -> Result ()
checkIfAvailableVarInEnv id env pos = do
    let i = Set.member id (names env)
    case i of
        False -> return ()
        True  -> throwError $ show $ ErrorUsedName "argument" id pos

ensureMyType pos t etype = do
    when (t /= etype) $ throwError $ show $ ErrorTypeMismatch etype t pos

ensureBasicType pos t = do
    case t of
        MyInt -> return()
        MyBool -> return()
        MyStr -> return()
        _ -> throwError $ show $ ErrorTypeMismatch1 "int, bool or string" t pos

checkPrintArgs [] pos nb = return True
checkPrintArgs (expr:exprs) pos nb = do
    when (expr == MyVoid) $ throwError $ show $ ErrorPrint nb pos
    checkPrintArgs exprs pos (nb+1)

transIdent :: G.Ident -> Result String
transIdent (G.Ident string) = return string

transType ::  G.Type -> Result Type
transType x = case x of
    G.MyInt _ -> return MyInt
    G.MyStr _ -> return MyStr
    G.MyBool _ -> return MyBool
    G.MyVoid _ -> return MyVoid

transTypeNotVoid ::  G.Type -> Result Type
transTypeNotVoid x = case x of
    G.MyInt _ -> return MyInt
    G.MyStr _ -> return MyStr
    G.MyBool _ -> return MyBool
    G.MyVoid pos -> throwError $ show $ ErrorVoid pos
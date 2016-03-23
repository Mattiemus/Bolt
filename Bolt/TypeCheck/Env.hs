{-# LANGUAGE TupleSections #-}

module Bolt.TypeCheck.Env where

import Data.Maybe
import Data.Foldable

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

import qualified Bolt.AST.Syntax as P

--------------------------------------------------------------------------------
-- Type checking environment
--------------------------------------------------------------------------------

data Env
    = Env {
            envContainer :: Type,
            envTypes :: M.Map Identifier Type,
            envScope :: M.Map P.Identifier Type
        }
    deriving (Show)

defaultEnv :: Env
defaultEnv = Env {
        envContainer = TUnknown,
        envTypes = M.fromList [
                ("Boolean", TBoolean),
                ("ISize", TISize), ("ILong", TILong), ("Int8", TInt8), ("Int16", TInt16), ("Int32", TInt32), ("Int64", TInt64),
                ("UISize", TUISize), ("UILong", TUILong), ("UInt8", TUInt8), ("UInt16", TUInt16), ("UInt32", TUInt32), ("UInt64", TUInt64),
                ("Float32", TFloat32), ("Float64", TFloat64)
            ],
        envScope = M.empty
    }

data TypeError
    = TypeRedeclared Identifier
    | TypeMismatch Type Type
    | ParamLengthMismatch Int
    | NotInScope Identifier
    | ExpectedFunction Type
    | Msg String
    deriving (Show)

type TypeCheck a = StateT Env (Except TypeError) a

runTypeCheck :: TypeCheck a -> Either TypeError (a, Env)
runTypeCheck tc = runExcept (runStateT tc defaultEnv)

execTypeCheck :: TypeCheck a -> Either TypeError Env
execTypeCheck tc = runExcept (execStateT tc defaultEnv)

runSubTypeCheck :: TypeCheck a -> TypeCheck (a, Env)
runSubTypeCheck tc = do
    currEnv <- get
    either throwError return (runExcept (runStateT tc currEnv))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Identifier = String

data Type
    = TUnknown | TToInfer
    | TBoolean
    | TISize | TILong | TInt8 | TInt16 | TInt32 | TInt64
    | TUISize | TUILong | TUInt8 | TUInt16 | TUInt32 | TUInt64
    | TFloat32 | TFloat64
    | TPrim Identifier
    | Type `TUnion` Type
    | Type `TIntersection` Type
    | TFunc Type [Type]
    deriving (Show, Eq)

numberT :: Type
numberT = integerT `TUnion` floatT

integerT :: Type
integerT = TInt32 -- TODO: signedT `TUnion` unsignedT

signedT :: Type
signedT = TISize `TUnion` TILong `TUnion` TInt8 `TUnion` TInt16 `TUnion` TInt32 `TUnion` TInt64

unsignedT :: Type
unsignedT = TUISize `TUnion` TUILong `TUnion` TUInt8 `TUnion` TUInt16 `TUnion` TUInt32 `TUnion` TUInt64

floatT :: Type
floatT = TFloat32 `TUnion` TFloat64

assertType :: Type -> Type -> TypeCheck ()
assertType expectedTy actualT = when (expectedTy /= actualT) (lift (throwError (TypeMismatch expectedTy actualT)))

registerType :: Identifier -> Type -> TypeCheck ()
registerType name ty = do
    env @ Env { envTypes = types } <- get
    case M.lookup name types of
        Just currTy ->
            unless (currTy == ty) (throwError (TypeRedeclared name))
        Nothing ->
            put env {
                envTypes = M.insert name ty types
            }

lookupType :: P.Type -> TypeCheck Type
lookupType ty = do
    Env { envTypes = types } <- get
    case ty of
        P.Named name ->
            return (fromMaybe TUnknown (M.lookup name types))
        ty1 `P.Union` ty2 ->
            TUnion <$> lookupType ty2 <*> lookupType ty1
        ty1 `P.Intersection` ty2 ->
            TIntersection <$> lookupType ty2 <*> lookupType ty1

lookupMaybeType :: Maybe P.Type -> TypeCheck Type
lookupMaybeType Nothing = return TToInfer
lookupMaybeType (Just ty) = lookupType ty

--------------------------------------------------------------------------------
-- Scoped objects
--------------------------------------------------------------------------------

addToScope :: Identifier -> Type -> TypeCheck ()
addToScope name objType = do
    env @ Env { envScope = scope } <- get
    case M.lookup name scope of
        Just _ ->
            error "addToScope: unhandled"
        Nothing ->
            put env {
                envScope = M.insert name objType scope
            }

lookupScope :: Identifier -> TypeCheck Type
lookupScope name = do
    Env { envScope = scope } <- get
    case M.lookup name scope of
        Nothing -> throwError (NotInScope name)
        Just ty -> return ty

--------------------------------------------------------------------------------
-- Declaration checker
--------------------------------------------------------------------------------

checkDecl :: P.Decl -> TypeCheck ()
checkDecl decl =
    case decl of
        P.TypeAlias name ty -> do
            aliasTy <- lookupType ty
            registerType name aliasTy
        P.Prim name -> do
            let primTy = TPrim name
            registerType name primTy
            addToScope name primTy
        P.Var (name, ty, _) -> do
            aliasTy <- lookupMaybeType ty
            addToScope name aliasTy
        P.Fun name ty params bodyStmt -> do
            retTy <- lookupMaybeType ty
            paramTys <- mapM (\(n, t, _) -> (n, ) <$> lookupMaybeType t) params
            (_, env) <- runSubTypeCheck $ do
                state (\env -> ((), env { envContainer = retTy }))
                addToScope name (TFunc retTy (snd <$> paramTys))
                mapM_ (uncurry addToScope) paramTys
                mapM_ checkStmt bodyStmt
            addToScope name (TFunc (envContainer env) (snd <$> paramTys))

--------------------------------------------------------------------------------
-- Statement checker
--------------------------------------------------------------------------------

checkStmt :: P.Stmt -> TypeCheck ()
checkStmt stmt =
    case stmt of
        P.Expr expr -> void (checkExpr expr)
        P.Ret expr -> do
            Env { envContainer = container } <- get
            exprTy <- checkExpr expr
            assertType container exprTy
        P.Cond condE thenStmt mayElseStmt -> do
            condTy <- checkExpr condE
            assertType TBoolean condTy
            mapM_ checkStmt thenStmt
            traverse_ (mapM_ checkStmt) mayElseStmt

--------------------------------------------------------------------------------
-- Expression checker
--------------------------------------------------------------------------------

checkExpr :: P.Expr -> TypeCheck Type
checkExpr expr =
    case expr of
        P.Lit (P.Int _) -> return integerT
        P.Lit (P.Bool _) -> return TBoolean
        P.Lit (P.Char _) -> error "checkExpr: Char type is not implemented"
        P.Lit (P.String _) -> error "checkExpr: String type is not implemented"
        P.Symbol name -> lookupScope name
        P.BinOp opName lhs rhs -> do
            lhsTy <- checkExpr lhs
            rhsTy <- checkExpr rhs
            checkBinOp opName lhsTy rhsTy
        P.UnOp opType opName value -> error "checkExpr: unop not implemented"
        P.Call funcName params -> do
            ty <- lookupScope funcName
            case ty of
                TFunc actRetTy actParamTys -> do
                    paramTys <- mapM checkExpr params
                    if length paramTys /= length actParamTys
                        then throwError (ParamLengthMismatch (length actParamTys))
                        else do
                            mapM_ (uncurry assertType) (zip actParamTys paramTys)
                            return actRetTy
                _ -> throwError (ExpectedFunction ty)


checkBinOp :: String -> Type -> Type -> TypeCheck Type
checkBinOp opName lhs rhs =
    case opName of
        "||" -> do
            assertType TBoolean lhs
            assertType TBoolean rhs
            return TBoolean
        "==" -> do
            assertType lhs rhs
            return TBoolean
        "+" -> do
            assertType TInt32 lhs
            assertType TInt32 rhs
            return TInt32
        "-" -> do
            assertType TInt32 lhs
            assertType TInt32 rhs
            return TInt32
        _ -> error "checkBinOp: unknown op"

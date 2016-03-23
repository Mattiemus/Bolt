module Bolt.AST.Syntax where

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

type Identifier = String

data Type
    = Named Identifier
    | Type `Union` Type
    | Type `Intersection` Type
    deriving (Show, Eq)

data Lit
    = Int Integer
    | Bool Bool
    | Char Char
    | String String
    deriving (Show)

type Operator = String

data UnOpType = PostfixOp | PrefixOp
    deriving (Show)

data Expr
    = Lit Lit
    | Symbol Identifier
    | BinOp Operator Expr Expr
    | UnOp UnOpType Operator Expr
    | Call Identifier [Expr]
    deriving (Show)

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Stmt
    = Expr Expr
    | Ret Expr
    | Cond Expr [Stmt] (Maybe [Stmt])
    deriving (Show)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type VarDecl = (Identifier, Maybe Type, Maybe Expr)

data Decl
    = Var VarDecl
    | Fun Identifier (Maybe Type) [VarDecl] [Stmt]
    | Prim Identifier
    | TypeAlias Identifier Type
    deriving (Show)

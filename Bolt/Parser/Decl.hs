module Bolt.Parser.Decl where

import Text.Parsec
import Text.Parsec.Text

import Bolt.Parser.Style
import Bolt.Parser.Expr
import Bolt.Parser.Stmt
import Bolt.AST.Syntax

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

decl :: Parser Decl
decl =  fun
    <|> prim
    <|> typeAlias

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

var :: Parser VarDecl
var = do
    name <- identifier
    typ <- optionMaybe typing
    value <- optionMaybe (reservedOp "=" >> expr)
    return (name, typ, value)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

fun :: Parser Decl
fun = do
    reserved "fun"
    name <- identifier
    args <- parens (commaSep var)
    typ <- optionMaybe typing
    body <- stmtBlock
    return $ Fun name typ args body

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------

prim :: Parser Decl
prim = do
    reserved "primitive"
    name <- identifier
    return (Prim name)

--------------------------------------------------------------------------------
-- Type alias
--------------------------------------------------------------------------------

typeAlias :: Parser Decl
typeAlias = do
    reserved "type"
    name <- identifier
    reservedOp "="
    typ <- ty
    return (TypeAlias name typ)

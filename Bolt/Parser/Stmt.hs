module Bolt.Parser.Stmt where

import Text.Parsec
import Text.Parsec.Text

import Bolt.Parser.Style
import Bolt.Parser.Expr
import Bolt.AST.Syntax

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

stmt :: Parser Stmt
stmt =  returnS
    <|> cond
    <|> exprS

stmtBlock :: Parser [Stmt]
stmtBlock = braces (many stmt)
        <|> do { s <- stmt; return [s]; }

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

exprS :: Parser Stmt
exprS = do
    e <- expr
    semi
    return (Expr e)

returnS :: Parser Stmt
returnS = do
    reserved "return"
    e <- expr
    semi
    return (Ret e)

cond :: Parser Stmt
cond = do
    reserved "if"
    condExpr <- parens expr
    thenBody <- stmtBlock
    elseBody <- optionMaybe (reserved "else" >> stmtBlock)
    return (Cond condExpr thenBody elseBody)

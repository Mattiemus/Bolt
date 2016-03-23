module Bolt.Parser.Expr where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr

import Bolt.Parser.Style
import Bolt.AST.Syntax

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"
    where
        table = [[postfixOp "++", postfixOp "--"],
                 [prefixOp "++", prefixOp "--"],
                 [infixOp "*" AssocRight, infixOp "/" AssocRight],
                 [infixOp "+" AssocRight, infixOp "-" AssocRight],
                 [infixOp "==" AssocRight],
                 [infixOp "&&" AssocRight],
                 [infixOp "||" AssocRight]]
        term = parens expr <|> literal <|> try call <|> symbol
        postfixOp op = Postfix (reservedOp op >> return (UnOp PostfixOp op))
        prefixOp op = Prefix (reservedOp op >> return (UnOp PrefixOp op))
        infixOp op = Infix (reservedOp op >> return (BinOp op))

symbol :: Parser Expr
symbol = Symbol <$> identifier

literal :: Parser Expr
literal = Lit <$> (intLit <|> booleanLit <|> charLit <|> stringLit)

intLit :: Parser Lit
intLit = Int <$> integerLiteral

booleanLit :: Parser Lit
booleanLit = Bool <$> booleanLiteral

charLit :: Parser Lit
charLit = Char <$> charLiteral

stringLit :: Parser Lit
stringLit = String <$> stringLiteral

call :: Parser Expr
call = Call <$> identifier <*> parens (commaSep expr)

--------------------------------------------------------------------------------
-- Identifiers and Types
--------------------------------------------------------------------------------

ty :: Parser Type
ty =    parens unionTy
    <|> unionTy
    where
        unionTy = interscTy `chainl1` (Union <$ reservedOp "|")
        interscTy = namedTy `chainl1` (Intersection <$ reservedOp "&")
        namedTy = Named <$> identifier

typing :: Parser Type
typing = colon >> ty

module Bolt.Parser.Style where

import Control.Monad (void)

import Data.Functor.Identity
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok

import Bolt.AST.Syntax

--------------------------------------------------------------------------------
-- Parser style config
--------------------------------------------------------------------------------

style :: Tok.GenLanguageDef T.Text u Identity
style = Tok.LanguageDef
    {
        Tok.commentStart    = "/*",
        Tok.commentEnd      = "*/",
        Tok.nestedComments  = True,
        Tok.identStart      = letter <|> char '_',
        Tok.identLetter     = alphaNum <|> oneOf "_'",
        Tok.opStart         = oneOf "!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter        = oneOf "!#$%&*+./<=>?@\\^|-~",
        Tok.caseSensitive   = True,
        Tok.commentLine     = "//",
        Tok.reservedOpNames = [":", ";", "|", "&", "=",
                               "+", "++", "-", "--", "==", "*", "/", "||", "&&"],
        Tok.reservedNames   = ["fun", "primitive", "type",
                               "if", "else", "return",
                               "true", "false"]
    }

lexer :: Tok.GenTokenParser T.Text u Identity
lexer = Tok.makeTokenParser style

--------------------------------------------------------------------------------
-- Primitive parsers
--------------------------------------------------------------------------------

identifier :: Parser Identifier
identifier = Tok.identifier lexer

integerLiteral :: Parser Integer
integerLiteral = Tok.integer lexer

booleanLiteral :: Parser Bool
booleanLiteral = (True <$ reserved "true") <|> (False <$ reserved "false")

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

--------------------------------------------------------------------------------
-- Basic parsers
--------------------------------------------------------------------------------

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser ()
semi = void $ Tok.semi lexer

colon :: Parser ()
colon = void $ Tok.colon lexer

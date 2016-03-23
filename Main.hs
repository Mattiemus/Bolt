module Main where

import qualified Data.Text as T

import Text.Parsec

import qualified Bolt.Parser.Decl as P
import qualified Bolt.AST.Syntax as P
import qualified Bolt.TypeCheck.Env as T

main :: IO ()
main = return ()

parseIt :: String -> IO (Either ParseError [P.Decl])
parseIt fname = do
    let p = many1 P.decl <* eof
    input <- readFile fname
    return (runParser p () fname (T.pack input))

testP :: IO (Either ParseError [P.Decl])
testP = parseIt "test.bolt"

testT :: IO ()
testT = do
    pres <- testP
    case pres of
        Left err -> print err
        Right decls -> print (T.execTypeCheck (mapM T.checkDecl decls))

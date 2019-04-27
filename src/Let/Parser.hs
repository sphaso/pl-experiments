{-# LANGUAGE OverloadedStrings #-}

module Let.Parser where

import Control.Monad (void)
import Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Char

import Let.Types

newlines :: Parser ()
newlines = do
    char '\n'
    return ()

skip :: Parser ()
skip = optional . skipMany1 $ newlines

number :: Parser Expression
number = Number <$> read <$> many1 digit

variable :: Parser Expression
variable = Var <$> manyTill anyChar (void spaces)

minus :: Parser Expression
minus = do
    char '-'
    optional spaces
    char '('
    arg1 <- number <|> variable
    optional spaces
    char ','
    optional spaces
    arg2 <- number <|> variable
    optional spaces
    char ')'
    pure $ Minus arg1 arg2

expression :: Parser Expression
expression = minus

sourceCode :: Parser Expression
sourceCode = skip *> expression <* (skip <|> eof)

parsing text = case parse sourceCode "Let" text of
                 Left err -> error (show err)
                 Right code -> code

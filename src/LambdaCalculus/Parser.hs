module LambdaCalculus.Parser where

import Control.Applicative
import Data.Char

import LambdaCalculus.Types.LCExpr (ArithOp (..), LCExpr (..))
import Parser

type ParsedLCExpr = LCExpr String

parseLCExpr :: String -> Maybe ParsedLCExpr
parseLCExpr s = case runParser (lcExprParser <* many (satisfy isSpace)) s of
  Just (lcExpr, []) -> Just lcExpr
  _ -> Nothing

tokenized :: Parser a -> Parser a
tokenized aParser = many (satisfy isSpace) *> aParser

lcExprParser :: Parser ParsedLCExpr
lcExprParser = additiveParser

additiveParser :: Parser ParsedLCExpr
additiveParser = chainl1 additiveOpParser multiplicativeParser
 where
  additiveOpParser :: Parser (ParsedLCExpr -> ParsedLCExpr -> ParsedLCExpr)
  additiveOpParser = tokenized $ ArithBinOp <$> ((Add <$ char '+') <|> (Sub <$ char '-'))

multiplicativeParser :: Parser ParsedLCExpr
multiplicativeParser = chainl1 multiplicativeOpParser appParser
 where
  multiplicativeOpParser :: Parser (ParsedLCExpr -> ParsedLCExpr -> ParsedLCExpr)
  multiplicativeOpParser = tokenized $ ArithBinOp <$> ((Mul <$ char '*') <|> (Div <$ char '/'))

appParser :: Parser ParsedLCExpr
appParser = chainl1 (App <$ some (satisfy isSpace)) baseExprParser

baseExprParser :: Parser ParsedLCExpr
baseExprParser =
  varParser
    <|> intParser
    <|> absParser
    <|> (tokenized (char '(') *> lcExprParser <* tokenized (char ')'))

identParser :: Parser String
identParser = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

varParser :: Parser ParsedLCExpr
varParser =
  Var <$> tokenized identParser

absParser :: Parser ParsedLCExpr
absParser =
  Abs
    <$> (tokenized (char '\\') *> tokenized identParser)
    <*> (tokenized (string "->") *> lcExprParser)

intParser :: Parser ParsedLCExpr
intParser =
  Int . read @Int <$> tokenized (some (satisfy isDigit))

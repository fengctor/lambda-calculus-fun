{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Data.Bifunctor (first)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f aParser = Parser $ \s -> fmap (first f) (runParser aParser s)

instance Applicative Parser where
  pure a = Parser $ \s -> pure (a, s)

  fParser <*> aParser = Parser $ \s -> do
    (f, s') <- runParser fParser s
    (a, s'') <- runParser aParser s'
    pure (f a, s'')

instance Monad Parser where
  aParser >>= f = Parser $ \s -> do
    (a, s') <- runParser aParser s
    runParser (f a) s'

instance Alternative Parser where
  empty = Parser $ const empty

  aParser1 <|> aParser2 = Parser $ \s -> case runParser aParser1 s of
    Nothing -> runParser aParser2 s
    results -> results

token :: (Char -> Maybe a) -> Parser a
token f = Parser $ \case
  [] -> empty
  c : cs -> case f c of
    Nothing -> empty
    Just a -> pure (a, cs)

char :: Char -> Parser Char
char targetC = token $ \c -> if c == targetC then Just c else Nothing

string :: String -> Parser String
string = traverse char

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = token $ \c -> if p c then Just c else Nothing

chainl1 :: forall a. Parser (a -> a -> a) -> Parser a -> Parser a
chainl1 opParser aParser = aParser >>= go
 where
  go :: a -> Parser a
  go !a1 =
    ( do
        op <- opParser
        a2 <- aParser
        go (op a1 a2)
    )
      <|> pure a1

module Parser where

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser f'
        where
            f' input = do
                (input', x) <- p input
                Just (input', f $ x)

instance Applicative Parser where
    pure x = Parser f
        where 
            f input = Just (input, x)

    (Parser p1) <*> (Parser p2) = Parser f
        where 
            f input = do
                (input', f') <- p1 input
                (input'', a) <- p2 input'
                Just (input'', f' $ a)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser f
        where
            f input = p1 input <|> p2 input

charParser :: Char -> Parser Char
charParser ch = Parser f
    where
        f :: String -> Maybe (String, Char)
        f (x : xs)
            | x == ch = Just (xs, x)
            | otherwise = Nothing
        f [] = Nothing

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser
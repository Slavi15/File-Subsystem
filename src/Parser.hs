module Parser where

import Command (Command (..), MKCommands (MkDir, Touch))
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isSpace)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser f'
        where
            f' input = do
                (input', x) <- p input
                Just (input', f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser f
        where
            f input = Just (input, x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser f
        where
            f input = do
                (input', f') <- p1 input
                (input'', a) <- p2 input'
                Just (input'', f' a)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
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
stringParser = traverse charParser

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser f'
    where
        f' input =
            let (token, rest) = span f input
            in Just (rest, token)

ws :: Parser String
ws = spanParser isSpace

slashParser :: Parser String
slashParser = charParser '/' *> spanParser (/= '/') <* ws

wordParser :: String -> Maybe (String, String)
wordParser = runParser $ spanParser (/= ' ') <* ws

eofParser :: String -> Maybe (String, String)
eofParser = runParser $ spanParser (/= '$') <* charParser '$' <* ws

getNextDirectory :: String -> Maybe (String, String)
getNextDirectory "" = Just ("", "")
getNextDirectory path = runParser slashParser path

pwdParser :: Parser Command
pwdParser = PWDCommand <$ stringParser "pwd"

cdParser :: Parser Command
cdParser = CDCommand <$ stringParser "cd"

lsParser :: Parser Command
lsParser = LSCommand <$ stringParser "ls"

catParser :: Parser Command
catParser = CATCommand <$ stringParser "cat"

dirParser :: Parser Command
dirParser = f <$> (stringParser "mkdir" <|> stringParser "touch")
    where
        f "mkdir" = DIRCommand MkDir
        f "touch" = DIRCommand Touch
        f _ = undefined

rmParser :: Parser Command
rmParser = RMCommand <$ stringParser "rm"

quitParser :: Parser Command
quitParser = QUITCommand <$ stringParser ":q"

cmdParser :: Parser Command
cmdParser = pwdParser <|> cdParser <|> lsParser <|> catParser <|> dirParser <|> rmParser <|> quitParser

parseCommand :: String -> Maybe (String, Command)
parseCommand = runParser $ cmdParser <* ws

-- >>> runParser slashParser "/test/test1/test2"
-- Just ("/test1/test2","test")

-- >>> wordParser "/test/test1/test2 Test Content"
-- Just ("Test Content","/test/test1/test2")

-- >>> getNextDirectory "/test/test1/test2"
-- Just ("/test1/test2","test")

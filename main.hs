module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match " ++ show err
    Right val -> "Found value: " ++ input

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | Char Char
            | String String
            | Bool Bool

parseChar :: Parser LispVal
parseChar = do
            f <- oneOf "\\"
            s <- string "a" 
                <|> string "A"
                <|> string "("
                <|> string " " 
                <|> string "space" 
                <|> string "newline" 
            let t = [f] ++ [s]
            return $ String t

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many (anyChar)
            char '"'
            return $ String x

parseAtom = do
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
            x <- many1 digit
            return $ String x
            
-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

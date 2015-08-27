module Parser where

import Text.Parsec hiding (parse)
import qualified Text.Parsec (parse)
import Data.Text
import Data.Char
import Data.Functor
import Expr

type Parser = Parsec String ()

parseFile :: String -> IO Expr
parseFile path = parse path <$> readFile path

parse :: String -> String -> Expr
parse name input = case Text.Parsec.parse program name input of
  Right e -> e
  Left err -> error (show err)

program :: Parser Expr
program = do
  spaces
  e <- expr
  spaces
  eof
  return e

expr :: Parser Expr
expr = mathEx <|> term

term :: Parser Expr
term = choice
  [parens
  ,numlit
  ,strlit
  ,len
  ,decimal
  ,letbinder
  ,variable
  ]

len :: Parser Expr
len = try $ do
  l <- getL
  string "len("
  spaces
  e <- expr
  spaces
  char ')'
  spaces
  return (ELen l e)

decimal :: Parser Expr
decimal = try $ do
  l <- getL
  string "decimal("
  spaces
  e <- expr
  spaces
  char ')'
  spaces
  return (EDec l e)

letbinder :: Parser Expr
letbinder = try $ do
  l <- getL
  string "let"
  space
  spaces
  v <- ident
  spaces
  char '='
  spaces
  e1 <- expr
  spaces
  string "in"
  space
  spaces
  e2 <- expr
  return (EL l v e1 e2)

-- precedence
-- * 
-- + - 
-- ++

mathEx :: Parser Expr
mathEx =
  let factor1 = chainl1 term timesOp in
  let factor2 = chainl1 factor1 (plusOp <|> minusOp) in
  chainl1 factor2 catOp

catOp = do
  l <- getL
  string "++"
  spaces
  return (ECat l)

plusOp = try $ do
  l <- getL
  string "+"
  notFollowedBy (char '+')
  spaces
  return (EPlus l)

minusOp = do
  l <- getL
  string "-"
  spaces
  return (EMinus l)

timesOp = do
  l <- getL
  string "*"
  spaces
  return (ETimes l)

parens :: Parser Expr
parens = do
  char '('
  spaces
  e <- expr
  char ')'
  spaces
  return e

numlit :: Parser Expr
numlit = do
  l <- getL
  neg <- option 1 (char '-' >> return (-1))
  ds <- many1 digit
  spaces
  (return . EZ l . (*neg) . read) ds

strlit :: Parser Expr
strlit = do
  l <- getL
  char '"'
  content <- manyTill anyChar (char '"')
  spaces
  return (ES l (pack content))

variable :: Parser Expr
variable = do
  l <- getL
  v <- ident
  return (EV l v)

ident :: Parser Name
ident = do
  a <- letter
  bs <- many alphaNum
  spaces
  return (a:bs)

getL :: Parser Int
getL = do
  pos <- getPosition
  return (sourceLine pos)

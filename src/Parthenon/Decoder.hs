module Parthenon.Decoder
  ( boolean,
    double,
    string,
    integer,
    bigInt,
    array,
    struct,
    Athena (..),
  )
where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Parthenon.Types (Athena (..))
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, controlChar, markChar, space, spaceChar, symbolChar)
import Text.Megaparsec.Char.Lexer (decimal, float)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

struct :: [(Text, Parser Athena)] -> Parser Athena
struct entries' =
  null' <|> (AStruct <$> struct')
  where
    struct' :: Parser [(Text, Athena)]
    struct' = between leftBrace rightBrace entries

    entries :: Parser [(Text, Athena)]
    entries = sepBy (choice decoders) comma

    decoders :: [Parser (Text, Athena)]
    decoders = map decoder entries'

    decoder :: (Text, Parser Athena) -> Parser (Text, Athena)
    decoder (key, decoder') = do
      _ <- symbol key
      _ <- equal
      schema' <- decoder'
      pure (key, schema')

array :: Parser Athena -> Parser Athena
array decoder' = null' <|> (AArray <$> array')
  where
    array' :: Parser [Athena]
    array' = between leftSquare rightSquare (sepBy decoder' comma)

string :: Parser Athena
string = null' <|> (AString <$> characters)

integer :: Parser Athena
integer = null' <|> (AInt <$> decimal)

bigInt :: Parser Athena
bigInt = null' <|> (ABigInt <$> decimal)

double :: Parser Athena
double = null' <|> (ADouble <$> float)

boolean :: Parser Athena
boolean = null' <|> (ABoolean <$> boolean')
  where
    boolean' :: Parser Bool
    boolean' = false <|> true

    false :: Parser Bool
    false = symbol "false" $> False

    true :: Parser Bool
    true = symbol "true" $> True

null' :: Parser Athena
null' = do
  _ <- symbol "null"
  pure ANull

equal :: Parser Text
equal = symbol "="

leftBrace :: Parser Text
leftBrace = symbol "{"

rightBrace :: Parser Text
rightBrace = symbol "}"

leftSquare :: Parser Text
leftSquare = symbol "["

rightSquare :: Parser Text
rightSquare = symbol "]"

comma :: Parser Text
comma = symbol ","

characters :: Parser Text
characters = takeWhileP (Just "character") anyCharacterExceptReserved
  where
    anyCharacterExceptReserved :: Char -> Bool
    anyCharacterExceptReserved character = character `notElem` ['{', '}', '[', ']', ',']

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = Lexer.symbol space

module Parthenon.Decoder
  ( boolean,
    double,
    string,
    integer,
    bigInt,
    array,
    struct,
    specialString,
    Athena (..),
  )
where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void
import Parthenon.Types (Athena (..))
import Text.Megaparsec
import Text.Megaparsec.Char (space)
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
    entries = (choice decoders <|> unknownDecoder) `sepBy` comma

    decoders :: [Parser (Text, Athena)]
    decoders = map decoder entries'

    decoder :: (Text, Parser Athena) -> Parser (Text, Athena)
    decoder (key', decoder') = do
      _ <- try $ symbol key'
      _ <- equal
      schema' <- try decoder'
      pure (key', schema')

    unknownDecoder :: Parser (Text, Athena)
    unknownDecoder = do
      key' <- key
      _ <- equal
      schema' <- try string
      pure (key', schema')

    key :: Parser Text
    key = takeWhileP (Just "key") anyCharacterExceptReserved
      where
        anyCharacterExceptReserved :: Char -> Bool
        anyCharacterExceptReserved character = character `notElem` ['{', '}', '[', ']', ',', '=']

array :: Parser Athena -> Parser Athena
array decoder' = null' <|> (AArray <$> array')
  where
    array' :: Parser [Athena]
    array' = between leftSquare rightSquare (decoder' `sepBy` comma)

specialString :: Parser Athena
specialString = null'

string :: Parser Athena
string = null' <|> (AString <$> characters)
  where
    characters :: Parser Text
    characters = takeWhileP (Just "character") anyCharacterExceptReserved

    anyCharacterExceptReserved :: Char -> Bool
    anyCharacterExceptReserved character = character `notElem` ['{', '}', '[', ']', ',']

integer :: Parser Athena
integer = null' <|> (AInt <$> Lexer.signed space decimal)

bigInt :: Parser Athena
bigInt = null' <|> (ABigInt <$> Lexer.signed space decimal)

double :: Parser Athena
double = null' <|> (ADouble <$> Lexer.signed space float)

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

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = Lexer.symbol space

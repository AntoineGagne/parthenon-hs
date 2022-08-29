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
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
    entries = decoder `sepBy` comma

    decoder :: Parser (Text, Athena)
    decoder = do
      key' <- keys
      _ <- equal
      schema' <- try $ getDecoder key'
      pure (key', schema')

    keys :: Parser Text
    keys =
      let (rawKeys', _) = unzip entries'
          keys' = map symbol rawKeys'
       in (try (choice keys') <|> key)

    key :: Parser Text
    key = takeWhileP (Just "key") anyCharacterExceptReserved
      where
        anyCharacterExceptReserved :: Char -> Bool
        anyCharacterExceptReserved character = character `notElem` ['{', '}', '[', ']', ',', '=']

    getDecoder :: Text -> Parser Athena
    getDecoder key' = fromMaybe string $ decodersByKeys !? key'

    decodersByKeys :: Map Text (Parser Athena)
    decodersByKeys = Map.fromList entries'

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

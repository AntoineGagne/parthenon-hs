module Parthenon.Schema
  ( schema,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void
import qualified Parthenon.Decoder as Decoder
import Parthenon.Types (Athena (..), Precision, Scale)
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

schema :: Parser (Parser Athena)
schema = try (array <|> struct)

array :: Parser (Parser Athena)
array = do
  void $ symbol "array"
  encoder' <- betweenAngleBrackets encoder
  pure $ Decoder.array encoder'

struct :: Parser (Parser Athena)
struct = do
  void $ symbol "struct"
  encoders <- betweenAngleBrackets (sepBy keyValue comma)
  pure $ Decoder.struct encoders
  where
    keyValue :: Parser (Text, Parser Athena)
    keyValue = do
      key <- characters
      void $ symbol ":"
      decoder <- structEncoder
      pure (key, decoder)

structEncoder :: Parser (Parser Athena)
structEncoder =
  try
    ( integer
        <|> tinyInt
        <|> smallInt
        <|> bigInt
        <|> boolean
        <|> float
        <|> double
        <|> decimal
        <|> string
        <|> char
        <|> struct
        <|> array
    )
  where
    string :: Parser (Parser Athena)
    string = symbol "string" $> Decoder.structString

encoder :: Parser (Parser Athena)
encoder =
  try
    ( integer
        <|> tinyInt
        <|> smallInt
        <|> bigInt
        <|> boolean
        <|> float
        <|> double
        <|> decimal
        <|> string
        <|> char
        <|> struct
        <|> array
    )
  where
    string :: Parser (Parser Athena)
    string = symbol "string" $> Decoder.string

integer :: Parser (Parser Athena)
integer = try (symbol "int" <|> symbol "integer") $> Decoder.integer

tinyInt :: Parser (Parser Athena)
tinyInt = symbol "tinyint" $> Decoder.tinyInt

smallInt :: Parser (Parser Athena)
smallInt = symbol "smallint" $> Decoder.smallInt

bigInt :: Parser (Parser Athena)
bigInt = symbol "bigint" $> Decoder.bigInt

double :: Parser (Parser Athena)
double = symbol "double" $> Decoder.double

float :: Parser (Parser Athena)
float = symbol "float" $> Decoder.float

boolean :: Parser (Parser Athena)
boolean = symbol "boolean" $> Decoder.boolean

char :: Parser (Parser Athena)
char = do
  void $ symbol "char"
  length' <- betweenParenthesis Lexer.decimal
  pure $ Decoder.char length'

decimal :: Parser (Parser Athena)
decimal = do
  void $ symbol "decimal"
  (precision, scale) <- betweenParenthesis numbers
  pure $ Decoder.decimal precision scale
  where
    numbers :: Parser (Precision, Scale)
    numbers = do
      precision <- Lexer.decimal
      scale <- option 0 (symbol "," *> Lexer.decimal)
      pure (precision, scale)

betweenAngleBrackets :: Parser a -> Parser a
betweenAngleBrackets = between leftAngle rightAngle
  where
    leftAngle :: Parser Text
    leftAngle = symbol "<"

    rightAngle :: Parser Text
    rightAngle = symbol ">"

betweenParenthesis :: Parser a -> Parser a
betweenParenthesis = between leftParenthesis rightParenthesis
  where
    leftParenthesis :: Parser Text
    leftParenthesis = symbol "("

    rightParenthesis :: Parser Text
    rightParenthesis = symbol ")"

characters :: Parser Text
characters = takeWhileP (Just "character") anyCharacterExceptReserved
  where
    anyCharacterExceptReserved :: Char -> Bool
    anyCharacterExceptReserved character = character `notElem` ['<', '>', ':', ',']

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = Lexer.symbol space

comma :: Parser Text
comma = symbol ","

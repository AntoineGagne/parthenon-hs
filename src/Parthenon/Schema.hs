module Parthenon.Schema
  ( schema,
  )
where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, space)
import Text.Megaparsec.Char.Lexer (decimal, float)
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Schema
  = SStruct [(Text, Schema)]
  | SArray [Schema]
  | SInt Int
  | SString Text
  | SBigInt Integer
  | SDouble Double
  | SBoolean Bool
  deriving (Show)

type Parser = Parsec Void Text

type SchemaParser = Parsec Void Text Schema

schema :: Parser SchemaParser
schema = try (sArray <|> sStruct)

sArray :: Parser SchemaParser
sArray = do
  _ <- symbol "array"
  encoder <- betweenAngleBrackets sEncoder
  pure $
    SArray <$> do
      between leftSquare rightSquare $ sepBy encoder comma

sStruct :: Parser SchemaParser
sStruct = do
  _ <- symbol "struct"
  encoders <- sepBy sKeyValue comma
  pure $
    SStruct <$> do
      between leftBrace rightBrace $ sepBy (choice encoders) comma

sKeyValue :: Parser (Parser (Text, Schema))
sKeyValue = do
  key <- characters
  _ <- symbol ":"
  sStructEntry key <$> sEncoder

sStructEntry :: Text -> Parser Schema -> Parser (Text, Schema)
sStructEntry key encoder = do
  _ <- symbol key
  _ <- equal
  schema' <- encoder
  pure (key, schema')

sEncoder :: Parser SchemaParser
sEncoder = try (sInteger <|> sBigInt <|> sBoolean <|> sDouble <|> sString <|> sStruct <|> sArray)

betweenAngleBrackets :: Parser a -> Parser a
betweenAngleBrackets = between leftAngle rightAngle

sString :: Parser SchemaParser
sString = symbol "string" $> (SString <$> characters)

sInteger :: Parser SchemaParser
sInteger = symbol "int" $> (SInt <$> decimal)

sBigInt :: Parser SchemaParser
sBigInt = symbol "bigint" $> (SBigInt <$> decimal)

sDouble :: Parser SchemaParser
sDouble = symbol "double" $> (SDouble <$> float)

sBoolean :: Parser SchemaParser
sBoolean = symbol "boolean" $> (true <|> false)
  where
    false :: Parser Schema
    false = symbol "false" $> SBoolean False

    true :: Parser Schema
    true = symbol "true" $> SBoolean True

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

characters :: Parser Text
characters = Text.pack <$> some alphaNumChar

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = Lexer.symbol space

leftAngle :: Parser Text
leftAngle = symbol "<"

rightAngle :: Parser Text
rightAngle = symbol ">"

comma :: Parser Text
comma = symbol ","

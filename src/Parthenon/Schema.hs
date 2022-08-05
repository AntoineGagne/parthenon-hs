module Parthenon.Schema
  ( schema,
  )
where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Parthenon.Decoder (Athena)
import qualified Parthenon.Decoder as Decoder
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, space)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

schema :: Parser (Parser Athena)
schema = try (sArray <|> sStruct)

sArray :: Parser (Parser Athena)
sArray = do
  _ <- symbol "array"
  encoder <- betweenAngleBrackets sEncoder
  pure $ Decoder.array encoder

sStruct :: Parser (Parser Athena)
sStruct = do
  _ <- symbol "struct"
  encoders <- betweenAngleBrackets (sepBy sKeyValue comma)
  pure $ Decoder.struct encoders

sKeyValue :: Parser (Text, Parser Athena)
sKeyValue = do
  key <- characters
  _ <- symbol ":"
  decoder <- sEncoder
  pure (key, decoder)

sEncoder :: Parser (Parser Athena)
sEncoder = try (sInteger <|> sBigInt <|> sBoolean <|> sDouble <|> sString <|> sStruct <|> sArray)

betweenAngleBrackets :: Parser a -> Parser a
betweenAngleBrackets = between leftAngle rightAngle

sString :: Parser (Parser Athena)
sString = symbol "string" $> Decoder.string

sInteger :: Parser (Parser Athena)
sInteger = symbol "int" $> Decoder.integer

sBigInt :: Parser (Parser Athena)
sBigInt = symbol "bigint" $> Decoder.bigInt

sDouble :: Parser (Parser Athena)
sDouble = symbol "double" $> Decoder.double

sBoolean :: Parser (Parser Athena)
sBoolean = symbol "boolean" $> Decoder.boolean

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

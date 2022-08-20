module Parthenon.Schema
  ( schema,
  )
where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import qualified Parthenon.Decoder as Decoder
import Parthenon.Types (Athena (..))
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, space)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

schema :: Parser (Parser Athena)
schema = try (array <|> struct)

array :: Parser (Parser Athena)
array = do
  _ <- symbol "array"
  encoder' <- betweenAngleBrackets encoder
  pure $ Decoder.array encoder'

struct :: Parser (Parser Athena)
struct = do
  _ <- symbol "struct"
  encoders <- betweenAngleBrackets (sepBy keyValue comma)
  pure $ Decoder.struct encoders
  where
    keyValue :: Parser (Text, Parser Athena)
    keyValue = do
      key <- characters
      _ <- symbol ":"
      decoder <- encoder
      pure (key, decoder)

encoder :: Parser (Parser Athena)
encoder = try (integer <|> bigInt <|> boolean <|> double <|> string <|> struct <|> array)
  where
    string :: Parser (Parser Athena)
    string = symbol "string" $> Decoder.string

    integer :: Parser (Parser Athena)
    integer = symbol "int" $> Decoder.integer

    bigInt :: Parser (Parser Athena)
    bigInt = symbol "bigint" $> Decoder.bigInt

    double :: Parser (Parser Athena)
    double = symbol "double" $> Decoder.double

    boolean :: Parser (Parser Athena)
    boolean = symbol "boolean" $> Decoder.boolean

betweenAngleBrackets :: Parser a -> Parser a
betweenAngleBrackets = between leftAngle rightAngle

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

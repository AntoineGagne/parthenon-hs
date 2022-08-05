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
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, space)
import Text.Megaparsec.Char.Lexer (decimal, float)
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Athena
  = AStruct [(Text, Athena)]
  | AArray [Athena]
  | AInt Int
  | AString Text
  | ABigInt Integer
  | ADouble Double
  | ABoolean Bool
  deriving (Eq, Show)

type Parser = Parsec Void Text

struct :: [(Text, Parser Athena)] -> Parser Athena
struct entries' =
  AStruct <$> do
    between leftBrace rightBrace entries
  where
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
array decoder' = AArray <$> do between leftSquare rightSquare $ sepBy decoder' comma

string :: Parser Athena
string = AString <$> characters

integer :: Parser Athena
integer = AInt <$> decimal

bigInt :: Parser Athena
bigInt = ABigInt <$> decimal

double :: Parser Athena
double = ADouble <$> float

boolean :: Parser Athena
boolean = false <|> true
  where
    false :: Parser Athena
    false = symbol "false" $> ABoolean False

    true :: Parser Athena
    true = symbol "true" $> ABoolean True

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
characters = Text.pack <$> some alphaNumChar

symbol :: Tokens Text -> Parser (Tokens Text)
symbol = Lexer.symbol space

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
  = AStruct (Maybe [(Text, Athena)])
  | AArray (Maybe [Athena])
  | AInt (Maybe Int)
  | AString (Maybe Text)
  | ABigInt (Maybe Integer)
  | ADouble (Maybe Double)
  | ABoolean (Maybe Bool)
  deriving (Eq, Show)

type Parser = Parsec Void Text

struct :: [(Text, Parser Athena)] -> Parser Athena
struct entries' =
  AStruct <$> (null' <|> struct')
  where
    struct' :: Parser (Maybe [(Text, Athena)])
    struct' = Just <$> between leftBrace rightBrace entries

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
array decoder' = AArray <$> (null' <|> array')
  where
    array' :: Parser (Maybe [Athena])
    array' = Just <$> between leftSquare rightSquare (sepBy decoder' comma)

string :: Parser Athena
string = AString <$> (null' <|> (Just <$> characters))

integer :: Parser Athena
integer = AInt <$> (null' <|> Just <$> decimal)

bigInt :: Parser Athena
bigInt = ABigInt <$> (null' <|> Just <$> decimal)

double :: Parser Athena
double = ADouble <$> (null' <|> Just <$> float)

boolean :: Parser Athena
boolean = ABoolean <$> (null' <|> boolean')
  where
    boolean' :: Parser (Maybe Bool)
    boolean' = Just <$> (false <|> true)

    false :: Parser Bool
    false = symbol "false" $> False

    true :: Parser Bool
    true = symbol "true" $> True

null' :: Parser (Maybe a)
null' = do
  _ <- symbol "null"
  pure Nothing

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

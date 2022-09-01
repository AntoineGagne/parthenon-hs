module Parthenon.Decoder
  ( boolean,
    double,
    decimal,
    char,
    float,
    string,
    integer,
    tinyInt,
    smallInt,
    bigInt,
    array,
    struct,
    structString,
  )
where

import Control.Monad.Combinators
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Parthenon.Types (Athena (..), Length, Precision, Scale)
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

struct :: [(Text, Parser Athena)] -> Parser Athena
struct entries' =
  null' <|> (AStruct <$> struct')
  where
    struct' :: Parser [(Text, Athena)]
    struct' = between leftBrace rightBrace entries

    entries :: Parser [(Text, Athena)]
    entries = (try (choice decoders) <|> unknownDecoder) `sepBy` comma

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
      schema' <- try structString
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

structString :: Parser Athena
structString = null' <|> AString <$> characters
  where
    characters :: Parser Text
    characters = do
      input <- getInput
      let n = case (Text.findIndex (== '=') input, Text.findIndex (== '}') input) of
            (Nothing, Nothing) -> 0
            (Just _, Nothing) -> 0
            (Nothing, Just n') -> n'
            (Just m, Just n')
              | m < n' ->
                let untilEqual = Text.take m input
                    (taken', _) = Text.breakOnEnd "," untilEqual
                 in Text.length taken' - 1
            (Just _, Just n') ->
              n' - 1
      takeP (Just "characters") n

string :: Parser Athena
string = null' <|> (AString <$> characters)
  where
    characters :: Parser Text
    characters = takeWhileP (Just "character") anyCharacterExceptReserved

    anyCharacterExceptReserved :: Char -> Bool
    anyCharacterExceptReserved character = character `notElem` ['{', '}', '[', ']', ',']

integer :: Parser Athena
integer = null' <|> (AInt <$> Lexer.signed space Lexer.decimal)

tinyInt :: Parser Athena
tinyInt = null' <|> (ATinyInt <$> Lexer.signed space Lexer.decimal)

smallInt :: Parser Athena
smallInt = null' <|> (ASmallInt <$> Lexer.signed space Lexer.decimal)

bigInt :: Parser Athena
bigInt = null' <|> (ABigInt <$> Lexer.signed space Lexer.decimal)

double :: Parser Athena
double = null' <|> (ADouble <$> Lexer.signed space Lexer.float)

decimal :: Precision -> Scale -> Parser Athena
decimal precision scale = null' <|> (ADecimal precision scale <$> Lexer.signed space Lexer.float)

float :: Parser Athena
float = null' <|> (AFloat <$> Lexer.signed space Lexer.float)

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

char :: Length -> Parser Athena
char length' = null' <|> (AChar length' <$> takeP (Just "fixed characters") length')

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

{-# LANGUAGE DeriveGeneric #-}

module Parthenon.Types
  ( Athena (..),
  )
where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import GHC.Generics

data Athena
  = AStruct (Maybe [(Text, Athena)])
  | AArray (Maybe [Athena])
  | AInt (Maybe Int)
  | AString (Maybe Text)
  | ABigInt (Maybe Integer)
  | ADouble (Maybe Double)
  | ABoolean (Maybe Bool)
  deriving (Eq, Generic, Show)

instance ToJSON Athena where
  toJSON (AStruct (Just keyValues)) = object [fromText key .= toJSON value | (key, value) <- keyValues]
  toJSON (AStruct nothing) = toJSON nothing
  toJSON (AArray (Just values)) = toJSON $ map toJSON values
  toJSON (AArray nothing) = toJSON nothing
  toJSON (AInt value) = toJSON value
  toJSON (AString value) = toJSON value
  toJSON (ADouble value) = toJSON value
  toJSON (ABigInt value) = toJSON value
  toJSON (ABoolean value) = toJSON value

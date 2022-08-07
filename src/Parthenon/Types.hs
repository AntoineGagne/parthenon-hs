module Parthenon.Types
  ( Athena (..),
  )
where

import Data.Text (Text)

data Athena
  = AStruct (Maybe [(Text, Athena)])
  | AArray (Maybe [Athena])
  | AInt (Maybe Int)
  | AString (Maybe Text)
  | ABigInt (Maybe Integer)
  | ADouble (Maybe Double)
  | ABoolean (Maybe Bool)
  deriving (Eq, Show)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Model where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.SafeCopy
import           Data.Text

data Person = Person
  { personId        :: Int
  , personFirstName :: Text
  , personLastName  :: Text
  } deriving (Eq, Show, Ord)

$(deriveJSON defaultOptions ''Person)
$(deriveSafeCopy 0 'base ''Person)

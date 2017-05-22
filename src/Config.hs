module Config where

import Repository

import Data.Acid

data Config = Config
  { state :: AcidState PersonDB }

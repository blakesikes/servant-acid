module Main where

import Config
import Lib
import Repository

import Data.Acid

import qualified Data.Map as Map

main :: IO ()
main = do
  state <- openLocalStateFrom "db" (PersonDB Map.empty)
  let cfg = Config { state = state }
  startApp cfg
  closeAcidState state

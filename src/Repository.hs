{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies #-}
module Repository where

import           Model

import           Control.Monad.Reader (ask)
import           Data.Acid
import           Data.SafeCopy
import           Data.Text
import           Data.Typeable

import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

type Key = Int

data PersonDB = PersonDB !(Map.Map Key Person) deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''PersonDB)

dbInsertPerson :: Person -> Update PersonDB ()
dbInsertPerson person = do
  let key = personId person
  PersonDB m <- S.get
  S.put (PersonDB (Map.insert key person m))

dbGetPerson :: Key -> Query PersonDB (Maybe Person)
dbGetPerson key = do
  PersonDB m <- ask
  return (Map.lookup key m)

dbGetAllPerson :: Query PersonDB [Person]
dbGetAllPerson = do
  PersonDB m <- ask
  return $ Map.elems m

dbDeletePerson :: Key -> Update PersonDB ()
dbDeletePerson key = do
  (PersonDB m) <- S.get
  S.put (PersonDB (Map.delete key m))

dbUpdatePerson :: Key -> Person -> Update PersonDB ()
dbUpdatePerson key person = do
  PersonDB m <- S.get
  S.put (PersonDB (Map.insert key person m))

$(makeAcidic ''PersonDB ['dbGetPerson, 'dbGetAllPerson, 'dbInsertPerson, 'dbDeletePerson])

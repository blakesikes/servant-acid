{-# LANGUAGE DataKinds         #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    ) where

import           Api                      (API, DeletePerson, GetAllPerson,
                                           GetPerson, InsertPerson, api)
import           Config
import           Model
import           Repository

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader
import           Data.Acid
import           Data.Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import qualified Data.Map                 as Map

type ReaderHandler = ReaderT Config Handler

startApp :: Config -> IO ()
startApp cfg = run 8080 $ app cfg

app :: Config -> Application
app cfg = serve api (server cfg)

server :: Config -> Server API
server cfg = enter (runReaderTNat cfg) readerServerT

readerServerT :: ServerT API (ReaderT Config Handler)
readerServerT = getAllPerson :<|> getPerson :<|> insertPerson :<|> deletePerson

getAllPerson :: ReaderT Config Handler [Person]
getAllPerson = do
  (Config state) <- ask
  liftIO $ query state DbGetAllPerson

getPerson :: Int -> ReaderT Config Handler Person
getPerson pid = do
  (Config state) <- ask
  mbPerson <- liftIO $ query state (DbGetPerson pid)
  case mbPerson of
    Nothing -> throwError err404 { errBody = "No person with given id" }
    Just p -> return p

insertPerson :: Person -> ReaderT Config Handler ()
insertPerson p = do
  (Config state) <- ask
  liftIO $ update state (DbInsertPerson p)

deletePerson :: Int -> ReaderT Config Handler ()
deletePerson pid = do
  (Config state) <- ask
  liftIO $ update state (DbDeletePerson pid)

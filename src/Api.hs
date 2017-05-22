{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import           Model
import           Servant

type GetPerson = "person" :> Capture "pid" Int :> Get '[JSON] Person
type GetAllPerson = "person" :> Get '[JSON] [Person]
type InsertPerson = "person" :> ReqBody '[JSON] Person :> Post '[JSON] ()
type DeletePerson = "person" :> Capture "pid" Int :> Delete '[JSON] ()

type API = GetAllPerson
  :<|> GetPerson
  :<|> InsertPerson
  :<|> DeletePerson

api :: Proxy API
api = Proxy

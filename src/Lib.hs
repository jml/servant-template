{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( Port
  , Settings
  , defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import qualified Network.Wai.Middleware.RequestLogger as RL
import Servant ((:>), Get, JSON, Server, serve)

data User = User
  { _userId        :: Int
  , _userFirstName :: Text
  , _userLastName  :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = runSettings (warpSettings 8080) (RL.logStdoutDev app)

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Port -> Settings
warpSettings port =
  setBeforeMainLoop printPort (setPort port' defaultSettings)
  where
    printPort = putStrLn $ "hello-prometheus-haskell running at http://localhost:" ++ show port' ++ "/"
    port' = port


app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

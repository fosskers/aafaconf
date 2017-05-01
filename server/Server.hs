{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import           Data.Proxy
import           Database.SQLite.Simple
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Servant.API
import           Servant.Server
import           Servant.Utils.StaticFiles
import           Server.Database
import           Server.Types
import           System.Exit
import           System.Posix.Signals hiding (Handler)

---

type API =
       "register" :> Get '[HTML] B.ByteString
  :<|> "register" :> ReqBody '[JSON] Person      :> Post '[JSON] String
  :<|> "groups"   :> Get '[HTML] B.ByteString
  :<|> "groups"   :> Capture "block" Block       :> Get '[JSON] [Person]
  :<|> "groups"   :> ReqBody '[JSON] BlockSignin :> Post '[JSON] String
  :<|> "signin"   :> Get '[HTML] B.ByteString
  :<|> "signin"   :> ReqBody '[JSON] Int         :> Post '[JSON] String
  :<|> "everyone" :> Get '[JSON] [Person]
  :<|> "ping"     :> Get '[PlainText] String
  :<|> "assets"   :> Raw
  :<|> Get '[HTML] B.ByteString

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env =
       pure (day1 env) :<|> reg env
  :<|> pure (day2 env) :<|> pbb env :<|> day2H env
  :<|> pure (day3 env) :<|> sign env :<|> day3H env
  :<|> pure "pong"
  :<|> serveDirectory "assets"
  :<|> pure (day1 env)

app :: Env -> Application
app = serve api . server

reg :: Env -> Person -> Handler String
reg env p = liftIO (register (conn env) p) >> pure "Success"

sign :: Env -> Int -> Handler String
sign env uuid = liftIO (signin (conn env) uuid) >> pure "Success"

pbb :: Env -> Block -> Handler [Person]
pbb env b = liftIO $ peopleByBlock (conn env) b

day2H :: Env -> BlockSignin -> Handler String
day2H env b = liftIO (blockSignin (conn env) b) >> pure "Success"

day3H :: Env -> Handler [Person]
day3H env = liftIO . day3People $ conn env

main :: IO ()
main = do
  Args p <- getRecord "AAFA Conf Backend"
  putStrLn "Connecting to DB..."
  conn <- open "aafa.db"
  wake conn
  putStrLn "Reading compiled Elm files..."
  d1 <- B.readFile "assets/register.html"
  d2 <- B.readFile "assets/groups.html"
  d3 <- B.readFile "assets/signin.html"
  putStrLn "Listening for requests..."
  tid <- myThreadId
  let h = close conn >> putStrLn "Shutting down." >> E.throwTo tid ExitSuccess
  installHandler keyboardSignal (Catch h) Nothing
  installHandler softwareTermination (Catch h) Nothing
  W.run p (app $ Env conn d1 d2 d3)

{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import           Data.Proxy
import           Data.Text (Text)
import           Database.SQLite.Simple
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.Server
import           Server.Database
import           Server.Types
import           System.Exit
import           System.Posix.Signals hiding (Handler)

---

type API =
       "signin"   :> Get '[HTML] B.ByteString
  :<|> "signin"   :> ReqBody '[JSON] Int         :> Post '[JSON] Int
  :<|> "register" :> Get '[HTML] B.ByteString
  :<|> "register" :> ReqBody '[JSON] Person      :> Post '[JSON] Person
  :<|> "groups"   :> Get '[HTML] B.ByteString
  :<|> "groups"   :> ReqBody '[JSON] BlockSignin :> Post '[JSON] Text

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = file "signin.html" :<|> sign env
  :<|> file "register.html" :<|> reg env
  :<|> file "groups.html" :<|> day2 env

app :: Env -> Application
app = serve api . server

-- | Given a filepath, yield its contents as @text/html@.
file :: FilePath -> Handler B.ByteString
file = liftIO . B.readFile

reg :: Env -> Person -> Handler Person
reg env p = liftIO (register (conn env) p) >> pure p

sign :: Env -> Int -> Handler Int
sign env uuid = liftIO (signin (conn env) uuid) >> pure uuid

day2 :: Env -> BlockSignin -> Handler Text
day2 env b = liftIO (blockSignin (conn env) b) >> pure "Success."

main :: IO ()
main = do
  putStrLn "Connecting to DB..."
  conn <- open "aafa.db"
  wake conn
  putStrLn "Listening for requests..."
  tid <- myThreadId
  let h = close conn >> putStrLn "Shutting down." >> E.throwTo tid ExitSuccess
  installHandler keyboardSignal (Catch h) Nothing
  W.run 8081 (app $ Env conn)

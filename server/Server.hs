{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import           Data.Proxy
import qualified Network.HTTP.Media as M
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.Server

---

data HTML

type API = "foo" :> Get '[HTML] B.ByteString

instance Accept HTML where
  contentType _ = "text" M.// "html"

instance MimeRender HTML B.ByteString where
  mimeRender _ = id

api :: Proxy API
api = Proxy

server :: Server API
server = foo

app :: Application
app = serve api server

foo :: Handler B.ByteString
foo = liftIO $ B.readFile "index.html"

main :: IO ()
main = do
  putStrLn "Starting up..."
  W.run 8081 app
  putStrLn "Done."

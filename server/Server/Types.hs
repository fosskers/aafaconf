{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Types
  ( -- * HTTP
    HTML
    -- * Database Types
  , Person(..)
--  , Registration
    -- * Runtime Environment
  , Env(..)
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Database.SQLite.Simple
import qualified Network.HTTP.Media as M
import           Servant.API

---

data Env = Env { conn :: Connection }

-- | The Content-Type @text/html@.
data HTML

instance Accept HTML where
  contentType _ = "text" M.// "html"

instance MimeRender HTML B.ByteString where
  mimeRender _ = id

-- | An attendee of the conference.
data Person = Person { uuid     :: Int
                     , fname    :: Text
                     , lname    :: Text
                     , city     :: Text
                     , company  :: Text
                     , blockA   :: Maybe Text
                     , blockB   :: Maybe Text
                     , blockC   :: Maybe Text
                     , thirdDay :: Bool }

instance FromJSON Person where
  parseJSON (Object v) = Person
    <$> v .:  "uuid"
    <*> v .:  "fname"
    <*> v .:  "lname"
    <*> v .:  "city"
    <*> v .:  "company"
    <*> v .:? "blockA"
    <*> v .:? "blockB"
    <*> v .:? "blockC"
    <*> v .:? "thirdDay" .!= False

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Person where
  toRow (Person uu fn ln ci co ba bb bc td) = toRow (uu, fn, ln, ci, co, ba, bb, bc, td)

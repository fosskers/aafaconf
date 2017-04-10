{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Types
  ( -- * HTTP
    HTML
    -- * Database Types
  , Person(..)
  ) where

import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Database.SQLite.Simple
import qualified Network.HTTP.Media as M
import           Servant.API

---

-- | The Content-Type @text/html@.
data HTML

instance Accept HTML where
  contentType _ = "text" M.// "html"

instance MimeRender HTML B.ByteString where
  mimeRender _ = id

-- | An attendee of the conference.
data Person = Person { _id :: Int
                     , _fname :: Text
                     , _lname :: Text
                     , _city :: Text
                     , _company :: Text }

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field <*> field

instance ToRow Person where
  toRow (Person i fn ln ci co) = toRow (i, fn, ln, ci, co)

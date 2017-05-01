{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Server.Types
  ( -- * HTTP
    HTML
    -- * Database Types
  , Person(..)
  , Block(..)
  , pretty
  , BlockSignin(..)
    -- * Runtime Environment
  , Env(..)
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Database.SQLite.Simple
import           GHC.Generics
import qualified Network.HTTP.Media as M
import           Servant.API
import           Web.HttpApiData (FromHttpApiData)

---

data Env = Env { conn :: Connection
               , day1 :: B.ByteString
               , day2 :: B.ByteString
               , day3 :: B.ByteString
               }

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
                     , thirdDay :: Bool } deriving (Generic, FromJSON, ToJSON)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Person where
  toRow (Person uu fn ln ci co ba bb bc td) = toRow (uu, fn, ln, ci, co, ba, bb, bc, td)

data Block = A | B | C deriving (Generic, FromJSON)

instance FromHttpApiData Block where
  parseUrlPiece "A" = Right A
  parseUrlPiece "B" = Right B
  parseUrlPiece "C" = Right C
  parseUrlPiece _   = Left "Expected A, B, or C"

pretty :: Block -> String
pretty A = "blockA"
pretty B = "blockB"
pretty C = "blockC"

data BlockSignin = BlockSignin { block :: Block
                               , topic :: Text
                               , group :: [Int] } deriving (Generic, FromJSON)

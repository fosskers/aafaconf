{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Database where

import Database.SQLite.Simple
import Server.Types
import Text.Printf.TH

---

-- | Create the database if necessary.
wake :: Connection -> IO ()
wake c = execute_ c $ Query [st|
CREATE TABLE IF NOT EXISTS people (
uuid INTEGER PRIMARY KEY,
first_name TEXT,
last_name TEXT,
city TEXT,
company TEXT
);|]

-- | Register a new attendee.
register :: Connection -> Person -> IO ()
register c p = execute c "INSERT INTO people (uuid, first_name, last_name, city, company) values (?, ?, ?, ?, ?)" p

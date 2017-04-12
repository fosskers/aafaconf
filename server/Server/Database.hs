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
company TEXT,
blockA TEXT,
blockB TEXT,
blockC TEXT,
third_day BOOLEAN
);|]

-- | Register a new attendee.
register :: Connection -> Person -> IO ()
register c p = execute c q p
  where q = Query [st|
INSERT INTO people (
uuid, first_name, last_name, city, company, blockA, blockB, blockC, third_day
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
|]

-- | Mark the `Person` who corresponds to the given UUID as "present" for
-- the third day.
signin :: Connection -> Int -> IO ()
signin c uuid = execute c "UPDATE people SET third_day = 1 WHERE uuid = ?" $ Only uuid

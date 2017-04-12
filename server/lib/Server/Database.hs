{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Database where

import Data.Maybe (listToMaybe)
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

-- | Everyone who has registered.
people :: Connection -> IO [Person]
people c = query_ c "SELECT * FROM people;"

-- | Fetch one `Person`.
person :: Connection -> Int -> IO (Maybe Person)
person c uuid = listToMaybe <$> query c "SELECT * FROM people WHERE uuid = ?" (Only uuid)

-- | Register a new attendee.
register :: Connection -> Person -> IO ()
register c p = execute c q p
  where q = Query [st|
INSERT INTO people (
uuid, first_name, last_name, city, company, blockA, blockB, blockC, third_day
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)|]

-- | Mark the `Person` who corresponds to the given UUID as "present" for
-- the third day.
signin :: Connection -> Int -> IO ()
signin c uuid = execute c "UPDATE people SET third_day = 1 WHERE uuid = ?" $ Only uuid

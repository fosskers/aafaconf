{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Database where

import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Server.Types
import Text.Printf.TH

---

-- | Create the database and initial tables if necessary.
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

-- | Any `Person` who hasn't yet been signed in for a particular `Block`.
peopleByBlock :: Connection -> Block -> IO [Person]
peopleByBlock c b = query_ c (Query q)
  where q = [st|SELECT * FROM people WHERE %s IS NULL|] $ pretty b

-- | Fetch one `Person`.
person :: Connection -> Int -> IO (Maybe Person)
person c uuid = listToMaybe <$> query c "SELECT * FROM people WHERE uuid = ?" (Only uuid)

-- | Register a new attendee.
register :: Connection -> Person -> IO ()
register c = execute c q
  where q = Query [st|
INSERT INTO people (
uuid, first_name, last_name, city, company, blockA, blockB, blockC, third_day
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)|]

-- | Mark the `Person` who corresponds to the given UUID as "present" for
-- the third day.
signin :: Connection -> Int -> IO ()
signin c uuid = execute c "UPDATE people SET third_day = 1 WHERE uuid = ?" $ Only uuid

-- | Set a Block topic for a group of attendees.
blockSignin :: Connection -> BlockSignin -> IO ()
blockSignin c (BlockSignin b t ps) = execute c (Query q) $ Only t
  where q = [st|UPDATE people SET %s = ? WHERE uuid IN (%s)|] (pretty b) ps'
        ps' = intercalate "," $ map show ps

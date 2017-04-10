{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Database where

import Database.SQLite.Simple
import Text.Printf.TH

---

-- | Create the database if necessary.
wake :: Connection -> IO ()
wake c = execute_ c $ Query [st|
CREATE TABLE IF NOT EXISTS people (
id INTEGER PRIMARY KEY,
first_name TEXT,
last_name TEXT,
city TEXT,
company TEXT
);|]

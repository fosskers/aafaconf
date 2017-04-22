{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple
import Server.Database
import Server.Types
import Test.Tasty
import Test.Tasty.HUnit

---

main :: IO ()
main = do
  c <- open ":memory:"
  wake c
  defaultMain $ suite c
  close c

suite :: Connection -> TestTree
suite c = testGroup "AAFA Server Tests"
  [ testGroup "Database Interaction"
    [ testCase "Registration" $ writeGuy c
    , testCase "Sign-in" $ signinGirl c
    , testCase "Block Sign-in" $ day2Block c
    ]
  ]

guy :: Person
guy = Person 12345 "Jack" "TheCat" "Vancouver" "Horizon" Nothing Nothing Nothing False

girl :: Person
girl = Person 98765 "QTip" "TheCat" "Vancouver" "Horizon" Nothing Nothing Nothing False

peeps :: BlockSignin
peeps = BlockSignin A "Asskissing" [12345, 98765]

writeGuy :: Connection -> Assertion
writeGuy c = do
  register c guy
  ps <- people c
  length ps @?= 1

signinGirl :: Connection -> Assertion
signinGirl c = do
  register c girl
  signin c 98765
  p <- person c 98765
  fmap thirdDay p @?= Just True

day2Block :: Connection -> Assertion
day2Block c = do
  peeps <- peopleByBlock c A
  length peeps @?= 2
  blockSignin c . BlockSignin A "Asskissing" $ map uuid peeps
  p <- person c 98765
  fmap blockA p @?= Just (Just "Asskissing")

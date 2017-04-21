module Calls exposing
    ( -- * Types
      Person
    , Block(..)
    , BlockSignin
      -- * Json Encoding
    , encodePerson, encodeBlock, encodeBSI
      -- * API Calls
    , register, signin, groups
    )

import Array as A
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE

---

-- | A new registree.
type alias Person = { uuid    : Int
                    , fname   : String
                    , lname   : String
                    , city    : String
                    , company : String }

encodePerson : Person -> JE.Value
encodePerson p = JE.object [ ("uuid",    JE.int p.uuid)
                           , ("fname",   JE.string p.fname)
                           , ("lname",   JE.string p.lname)
                           , ("city",    JE.string p.city)
                           , ("company", JE.string p.company) ]

-- | A panel discussion time block.
type Block = A | B | C

encodeBlock : Block -> JE.Value
encodeBlock b = JE.string <| toString b

-- | A group of attendees to sign in on the second day.
type alias BlockSignin = { block : Block
                         , topic : String
                         , group : List Int }

encodeBSI : BlockSignin -> JE.Value
encodeBSI bsi = JE.object [ ("block", encodeBlock bsi.block)
                          , ("topic", JE.string bsi.topic)
                          , ("group", JE.array <| A.fromList <| List.map JE.int bsi.group) ]

register : String -> Person -> Http.Request String
register host p = Http.post (host ++ "/register") (Http.jsonBody <| encodePerson p) JD.string

signin : String -> Int -> Http.Request String
signin host uuid = Http.post (host ++ "/signin") (Http.jsonBody <| JE.int uuid) JD.string

groups : String -> BlockSignin -> Http.Request String
groups host bsi = Http.post (host ++ "/groups") (Http.jsonBody <| encodeBSI bsi) <| JD.string

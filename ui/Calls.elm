module Calls exposing (..)

import Array as A
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as J

---

-- | A new registree.
type alias Person = { uuid    : Int
                    , fname   : String
                    , lname   : String
                    , city    : String
                    , company : String }

encodePerson : Person -> J.Value
encodePerson p = J.object [ ("uuid", J.int p.uuid)
                          , ("fname", J.string p.fname)
                          , ("lname", J.string p.lname)
                          , ("city", J.string p.city)
                          , ("company", J.string p.company) ]

-- | A panel discussion time block.
type Block = A | B | C

encodeBlock : Block -> J.Value
encodeBlock b = J.string <| toString b

-- | A group of attendees to sign in on the second day.
type alias BlockSignin = { block : Block
                         , topic : String
                         , group : List Int }

encodeBSI : BlockSignin -> J.Value
encodeBSI bsi = J.object [ ("block", encodeBlock bsi.block)
                         , ("topic", J.string bsi.topic)
                         , ("group", J.array <| A.fromList <| List.map J.int bsi.group)
                         ]

--getFoo : Http.Request String
--getFoo = Http.getString "http://localhost:8081/ping"

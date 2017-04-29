module Calls exposing
    ( -- * Types
      Person
    , Block(..)
    , BlockSignin
      -- * Json Encoding
    , encodePerson, encodeBlock, encodeBSI
      -- * Json Decoding
    , decodePerson
      -- * API Calls
    , register, signin, groups, peopleFromBlock
    )

import Array as A
import Http
import Json.Decode as JD
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE
import Maybe as M

---

-- | A new registrant.
type alias Person = { uuid     : Int
                    , fname    : String
                    , lname    : String
                    , city     : String
                    , company  : String
                    , blockA   : Maybe String
                    , blockB   : Maybe String
                    , blockC   : Maybe String
                    , thirdDay : Bool }

encodePerson : Person -> JE.Value
encodePerson p = JE.object [ ("uuid",     JE.int p.uuid)
                           , ("fname",    JE.string p.fname)
                           , ("lname",    JE.string p.lname)
                           , ("city",     JE.string p.city)
                           , ("company",  JE.string p.company)
                           , ("blockA",   M.withDefault JE.null <| M.map JE.string p.blockA)
                           , ("blockB",   M.withDefault JE.null <| M.map JE.string p.blockB)
                           , ("blockC",   M.withDefault JE.null <| M.map JE.string p.blockC)
                           , ("thirdDay", JE.bool p.thirdDay) ]

decodePerson : JD.Decoder Person
decodePerson = decode Person
               |> required "uuid"     JD.int
               |> required "fname"    JD.string
               |> required "lname"    JD.string
               |> required "city"     JD.string
               |> required "company"  JD.string
               |> required "blockA"   (JD.nullable JD.string)
               |> required "blockB"   (JD.nullable JD.string)
               |> required "blockC"   (JD.nullable JD.string)
               |> required "thirdDay" JD.bool

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

-- | Everyone who hasn't been assigned a group for the given Block.
peopleFromBlock : String -> Block -> Http.Request (List Person)
peopleFromBlock host b = Http.get (host ++ "/groups/" ++ toString b) (JD.list decodePerson)

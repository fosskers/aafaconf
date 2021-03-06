module Test exposing (..)

import Calls exposing (..)
import Html exposing (..)
import Http as H

---

type Event = Res String (Result H.Error String) | Blk String (Result H.Error (List Person))

type alias State = { tests : List (Cmd Event)
                   , results : List Test }

main = Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }

init : (State, Cmd Event)
init = ( State (Maybe.withDefault [] <| List.tail tests) []
       , Maybe.withDefault Cmd.none <| List.head tests )

update : Event -> State -> (State, Cmd Event)
update event state =
    case event of
        Res l (Err e) -> (bad state l e , Maybe.withDefault Cmd.none <| List.head state.tests)
        Res l (Ok s)  -> (good state l s "Success", Maybe.withDefault Cmd.none <| List.head state.tests)
        Blk l (Err e) -> (bad state l e, Maybe.withDefault Cmd.none <| List.head state.tests)
        Blk l (Ok s)  -> (good state l (List.length s) 1, Maybe.withDefault Cmd.none <| List.head state.tests)

bad : State -> String -> H.Error -> State
bad s l e = { s | results = Test l (Bad <| toString e) :: s.results
            , tests = Maybe.withDefault [] <| List.tail s.tests }

good : State -> String -> a -> a -> State
good s l a b = { s | results = Test l (assert a b) :: s.results
               , tests = Maybe.withDefault [] <| List.tail s.tests }

view : State -> Html Event
view state = div [] <| List.map (\t -> div [] [ rth t ]) state.results

----------
-- TESTING
----------

type alias Test = { label : String, result : TestResult }

-- | The potential results of a test.
type TestResult = Bad String | Fail (String, String) | Pass

rth : Test -> Html b
rth t = let msg = case t.result of
  Bad s -> text <| "Test failed : " ++ s
  Fail (a,b) -> text <| "Assertion failed : GOT " ++ a ++ " - EXPECTED " ++ b
  Pass -> text "Test passed!"
        in div []
            [ text t.label
            , br [] []
            , text "→", msg ]

-- | All tests.
tests : List (Cmd Event)
tests = [ H.send (Res "register/") (register host guy)
        , H.send (Blk "groups/A/") (peopleFromBlock host A)
        , H.send (Res "groups/ (A)") (groups host <| BlockSignin A "Exploitation" [guy.uuid])
        , H.send (Res "groups/ (B)") (groups host <| BlockSignin B "Deception" [guy.uuid])
        , H.send (Res "groups/ (C)") (groups host <| BlockSignin C "Asskissing" [guy.uuid])
        , H.send (Res "signin/") (signin host guy.uuid)
        ]

-- | Assert that two values are equal.
assert : a -> a -> TestResult
assert a0 a1 = if a0 == a1 then Pass else Fail (toString a0, toString a1)

host : String
host = "http://localhost:8081"

guy : Person
guy = Person 987 "John" "Smith" "Yellowknife" "FooCorp" Nothing Nothing Nothing False

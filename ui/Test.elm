module Test exposing (..)

import Calls exposing (..)
import Html exposing (..)
import Http as H

---

type Event = Res String (Result H.Error String)

type alias State = { tests : List (Cmd Event)
                   , results : List (Test String) }

main = Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }

init : (State, Cmd Event)
init = ( State (Maybe.withDefault [] <| List.tail tests) []
       , Maybe.withDefault Cmd.none <| List.head tests )

update : Event -> State -> (State, Cmd Event)
update event state =
    case event of
        Res l (Err e) -> ({state | results = Test l (Bad <| toString e) :: state.results
                                 , tests = Maybe.withDefault [] <| List.tail state.tests
                          }, Maybe.withDefault Cmd.none <| List.head state.tests)
        Res l (Ok s)  -> ({state | results = Test l (assert s "Success") :: state.results
                                 , tests = Maybe.withDefault [] <| List.tail state.tests
                          }, Maybe.withDefault Cmd.none <| List.head state.tests)

view : State -> Html Event
view state = div [] <| List.map (\t -> div [] [ rth t ]) state.results

----------
-- TESTING
----------

type alias Test a = { label : String, result : TestResult a }

-- | The potential results of a test.
type TestResult a = Bad String | Fail (a,a) | Pass

rth : Test a -> Html b
rth t = let msg = case t.result of
  Bad s -> text <| "Test failed : " ++ s
  Fail (a,b) -> text <| "Assertion failed : GOT " ++ toString a ++ " - EXPECTED " ++ toString b
  Pass -> text "Test passed!"
        in div []
            [ text t.label
            , br [] []
            , text "→", msg ]

-- | All tests.
tests : List (Cmd Event)
tests = [ H.send (Res "register/") (register host guy)
        , H.send (Res "groups/ (A)") (groups host <| BlockSignin A "Exploitation" [guy.uuid])
        , H.send (Res "groups/ (B)") (groups host <| BlockSignin B "Deception" [guy.uuid])
        , H.send (Res "groups/ (C)") (groups host <| BlockSignin C "Asskissing" [guy.uuid])
        , H.send (Res "signin/") (signin host guy.uuid)
        ]

-- | Assert that two values are equal.
assert : a -> a -> TestResult a
assert a0 a1 = if a0 == a1 then Pass else Fail (a0, a1)

host : String
host = "http://localhost:8081"

guy : Person
guy = Person 987 "John" "Smith" "Yellowknife" "FooCorp" Nothing Nothing Nothing False

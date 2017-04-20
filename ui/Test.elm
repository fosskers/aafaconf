module Test exposing (main)

import Calls exposing (..)
import Html exposing (..)
import Http as H

---

type Event = Res (Result H.Error String)

type alias State = { results : List (TestResult String) }

main = Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }

init : (State, Cmd Event)
init = (State [], H.send Res (register host guy))

update : Event -> State -> (State, Cmd Event)
update event state =
    case event of
        Res (Err e) -> ({state | results = Bad (toString e) :: state.results}, Cmd.none)
        Res (Ok s)  -> ({state | results = assert s "Success" :: state.results}, Cmd.none)

view : State -> Html Event
view state = div [] <| List.map (\t -> div [] [ rth t]) state.results

----------
-- TESTING
----------

-- | The potential results of a test.
type TestResult a = Bad String | Fail (a,a) | Pass

rth : TestResult a -> Html b
rth tr = case tr of
  Bad s -> text <| "Test failed : " ++ s
  Fail (a,b) -> text <| "Assertion failed : GOT " ++ toString a ++ " - EXPECTED " ++ toString b
  Pass -> text "Test passed!"

-- | Assert that two values are equal.
assert : a -> a -> TestResult a
assert a0 a1 = if a0 == a1 then Pass else Fail (a0, a1)

host : String
host = "http://localhost:8081"

guy : Person
guy = Person 987 "John" "Smith" "Yellowknife" "FooCorp"

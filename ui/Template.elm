module Template exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

---

type Event = Foo

type alias State = { foo : String }

main = Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                    }

init : (State, Cmd Event)
init = (State "", Cmd.none)

update : Event -> State -> (State, Cmd Event)
update event state = (state, Cmd.none)

subscriptions : State -> Sub Event
subscriptions _ = Sub.none

view : State -> Html Event
view _ = text "Hi"

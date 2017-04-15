module Calls exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

---

type Event = Ping | Resp (Result Http.Error String)

type alias State = { resp  : String }

main = Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                    }

init : (State, Cmd Event)
init = (State "", Cmd.none)

update : Event -> State -> (State, Cmd Event)
update event state =
    case event of
        Ping -> (state, Http.send Resp getFoo)
        Resp (Err e) -> ({ state | resp = toString e}, Cmd.none)
        Resp (Ok  s) -> ({ state | resp = s}, Cmd.none)

subscriptions : State -> Sub Event
subscriptions _ = Sub.none

view : State -> Html Event
view state = div []
         [ button [ onClick Ping ] [ text "Click me"]
         , div [] [text state.resp]
         ]

getFoo : Http.Request String
getFoo = Http.getString "http://localhost:8081/ping"

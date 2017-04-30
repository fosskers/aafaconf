module Day1 exposing (..)

import Calls exposing (..)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as H
import Random as R
import Ui.Button as B
import Ui.Container as C


---


type Event
    = Fname String
    | Lname String
    | City String
    | Company String
    | RmId Int
    | Registered (Result H.Error String)
    | Submit


type alias State =
    { firstName : String
    , lastName : String
    , city : String
    , company : String
    , isSuccessful : Bool
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( State, Cmd Event )
init =
    ( State "" "" "" "" False, Cmd.none )


update : Event -> State -> ( State, Cmd Event )
update event state =
    case event of
        Fname jack ->
            ( { state | firstName = jack }, Cmd.none )

        Lname smith ->
            ( { state | lastName = smith }, Cmd.none )

        City hope ->
            ( { state | city = hope }, Cmd.none )

        Company abc ->
            ( { state | company = abc }, Cmd.none )

        Submit ->
            let
                fields =
                    [ state.firstName, state.lastName, state.city, state.company ]

                valid =
                    List.all (\s -> String.isEmpty s |> not) fields
            in
                if not valid then
                    ( state, Cmd.none )
                else
                    ( state, R.generate RmId (R.int 0 999999999) )

        RmId num ->
            let
                person =
                    Person num state.firstName state.lastName state.city state.company Nothing Nothing Nothing False
            in
                ( state, H.send Registered <| register "" person )

        Registered (Err _) ->
            ( state, Cmd.none )

        Registered (Ok _) ->
            ( { state | isSuccessful = True }, Cmd.none )


view : State -> Html Event
view state =
    pageLayout <| formLayout state


formLayout : State -> Html Event
formLayout state =
    if state.isSuccessful then
        centered [ h1 [] [ text <| "Thanks for registering, " ++ state.firstName ++ "!" ] ]
    else
        centered
            [ h3 [] [ text "Welcome to AAFA Santa Monica 2017!" ]
            , h5 [] [ text "Please register below." ]
            , div [] [ input [ placeholder "First Name", onInput Fname, maxlength 50 ] [] ]
            , div [] [ input [ placeholder "Last Name", onInput Lname, maxlength 50 ] [] ]
            , div [] [ input [ placeholder "City", onInput City, maxlength 50 ] [] ]
            , div [] [ input [ placeholder "Company", onInput Company, maxlength 50 ] [] ]
            , B.model "Submit" "primary" "medium" |> B.view Submit
            ]

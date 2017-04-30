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
import Ui.NotificationCenter as N


---


type Event
    = Fname String
    | Lname String
    | City String
    | Company String
    | RmId Int
    | Registered (Result H.Error String)
    | Submit
    | Notif N.Msg


type alias State =
    { firstName : String
    , lastName : String
    , city : String
    , company : String
    , isSuccessful : Bool
    , notif : N.Model Event
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
    let
        notif =
            N.init () |> N.timeout 5000 |> N.duration 500
    in
        ( State "" "" "" "" False notif, Cmd.none )


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
                    let
                        ( notif, cmd ) =
                            N.notify (text "Please fill out all the fields.") state.notif
                    in
                        ( { state | notif = notif }, Cmd.map Notif cmd )
                else
                    ( state, R.generate RmId (R.int 0 999999999) )

        RmId num ->
            let
                person =
                    Person num state.firstName state.lastName state.city state.company Nothing Nothing Nothing False
            in
                ( state, H.send Registered <| register "" person )

        Registered (Err _) ->
            let
                ( notif, cmd ) =
                    N.notify (text "Hmm, something went wrong! Please try again.") state.notif
            in
                ( { state | notif = notif }, Cmd.map Notif cmd )

        Registered (Ok _) ->
            ( { state | isSuccessful = True }, Cmd.none )

        Notif msg ->
            let
                ( notif, cmd ) =
                    N.update msg state.notif
            in
                ( { state | notif = notif }, Cmd.map Notif cmd )


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
            , N.view Notif state.notif
            ]

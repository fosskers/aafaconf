module Day1 exposing (..)

import Bootstrap.Button as BB
import Bootstrap.Form as BF
import Bootstrap.Form.Input as BI
import Bootstrap.Grid as G
import Bootstrap.Grid.Col as GC
import Bootstrap.Grid.Row as GR
import Calls exposing (..)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as H
import Navigation as Nav
import Random as R
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
    | Location Nav.Location


type alias State =
    { firstName : String
    , lastName : String
    , city : String
    , company : String
    , isSuccessful : Bool
    , notif : N.Model Event
    , loc : Nav.Location
    }


main =
    Nav.program Location
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Nav.Location -> ( State, Cmd Event )
init loc =
    let
        notif =
            N.init () |> N.timeout 5000 |> N.duration 500
    in
        ( State "" "" "" "" False notif loc, Cmd.none )


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
                ( state, H.send Registered <| register state.loc.origin person )

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

        Location loc ->
            ( { state | loc = loc }, Cmd.none )


view : State -> Html Event
view state =
    pageLayout <| formLayout state


formLayout : State -> Html Event
formLayout state =
    if state.isSuccessful then
        centered [ h1 [] [ text <| "Thanks for registering, " ++ state.firstName ++ "!" ] ]
    else
        G.container [ style [ ( "padding-top", "5%" ) ] ]
            [ G.row [ GR.centerXs ]
                [ G.col [ GC.xs10, GC.lg6 ]
                    [ div [] [ h3 [] [ text "Welcome to AAFA Santa Monica 2017!" ] ] ]
                ]
            , G.row [ GR.centerXs ]
                [ G.col [ GC.xs10, GC.lg6 ] [ h5 [] [ text "Please register below." ] ] ]
            , G.row [ GR.centerXs ]
                [ G.col [ GC.xs10, GC.lg6 ] [ form, N.view Notif state.notif ] ]
            , G.row [ GR.centerXs ]
                [ G.col [ GC.xs10, GC.lg6 ]
                    [ BB.button [ BB.primary, BB.block, BB.attrs [ onClick Submit ] ] [ text "Submit" ] ]
                ]
            ]


form : Html Event
form =
    BF.form []
        [ BF.group []
            [ BF.label [ for "i-fname" ] [ text "First Name" ]
            , BI.text [ BI.id "i-fname", BI.onInput Fname ]
            ]
        , BF.group []
            [ BF.label [ for "i-lname" ] [ text "Last Name" ]
            , BI.text [ BI.id "i-lname", BI.onInput Lname ]
            ]
        , BF.group []
            [ BF.label [ for "i-city" ] [ text "City" ]
            , BI.text [ BI.id "i-city", BI.onInput City ]
            ]
        , BF.group []
            [ BF.label [ for "i-company" ] [ text "Company" ]
            , BI.text [ BI.id "i-company", BI.onInput Company ]
            ]
        ]

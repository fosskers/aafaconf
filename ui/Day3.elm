module Day3 exposing (..)

import Bootstrap.Button as BB
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
import Set
import Ui.Container as C
import Ui.NotificationCenter as N


---


type Event
    = Typed String
    | Written (Result H.Error String)
    | FirstList (Result H.Error (List Person))
    | Notify N.Msg
    | Location Nav.Location
    | Chosen Person


type alias State =
    { isSuccessful : Bool
    , notify : N.Model Event
    , loc : Nav.Location
    , name : String
    , firstList : List Person
    }


main =
    Nav.program Location
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Nav.Location -> ( State, Cmd Event )
init loc =
    let
        notify =
            N.init () |> N.timeout 5000 |> N.duration 500
    in
        ( State False notify loc "" []
        , H.send FirstList <| day3People loc.origin
        )


update : Event -> State -> ( State, Cmd Event )
update event state =
    case event of
        FirstList (Err _) ->
            let
                ( notify, cmd ) =
                    N.notify (text "Something went wrong! Try refreshing the page.") state.notify
            in
                ( { state | notify = notify }, Cmd.map Notify cmd )

        FirstList (Ok ppl) ->
            ( { state | firstList = ppl }, Cmd.none )

        Typed n ->
            ( { state | name = n }, Cmd.none )

        Written (Err _) ->
            let
                ( notify, cmd ) =
                    N.notify (text "Something went wrong! Try signing in again.") state.notify
            in
                ( { state | notify = notify }, Cmd.map Notify cmd )

        Written (Ok _) ->
            ( { state | isSuccessful = True }, Cmd.none )

        Notify msg ->
            let
                ( notify, cmd ) =
                    N.update msg state.notify
            in
                ( { state | notify = notify }, Cmd.map Notify cmd )

        Location loc ->
            ( { state | loc = loc }, Cmd.none )

        Chosen p ->
            ( state, H.send Written <| signin state.loc.origin p.uuid )


subscriptions : State -> Sub Event
subscriptions _ =
    Sub.none


view : State -> Html Event
view state =
    pageLayout <| signInLayout state


signInLayout : State -> Html Event
signInLayout state =
    if state.isSuccessful then
        C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
            [ C.columnCenter []
                [ h1 [] [ text <| "Thanks for signing in!" ] ]
            ]
    else
        let
            matches =
                if String.length state.name >= 3 then
                    List.map (buttonify Chosen) <| List.filter (isMatch state) state.firstList
                else
                    []
        in
            G.container [ style [ ( "padding-top", "5%" ) ] ]
                [ G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                    [ G.col []
                        [ h3 []
                            [ text "Welcome to Day 3 of the AAFA Conference!"
                            , N.view Notify state.notify
                            ]
                        ]
                    ]
                , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                    [ G.col []
                        [ i [] [ text "To sign in, please type your name in the field below and click your name when it appears." ] ]
                    ]
                , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                    [ G.col []
                        [ BI.text [ BI.id "thing", BI.onInput Typed ] ]
                    ]
                , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                    [ G.col [] matches ]
                ]


isMatch : State -> Person -> Bool
isMatch state p =
    String.contains (String.toLower state.name) (String.toLower (p.fname ++ p.lname))


buttonify : (Person -> Event) -> Person -> Html Event
buttonify f p =
    BB.button [ BB.info, BB.attrs [ onClick <| f p ] ] [ text <| p.fname ++ " " ++ p.lname ]

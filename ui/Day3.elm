module Day3 exposing (..)

import Calls exposing (..)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as H
import Set
import Ui.Button as B
import Ui.Chooser as Ch
import Ui.Container as C
import Ui.NotificationCenter as N


---


type Event
    = Choosing Ch.Msg
    | SignIn
    | Written (Result H.Error String)
    | FirstList (Result H.Error (List Person))
    | Notify N.Msg


type alias State =
    { chooser : Ch.Model
    , isSuccessful : Bool
    , notify : N.Model Event
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( State, Cmd Event )
init =
    let
        chooser =
            Ch.init ()
                |> Ch.placeholder "Begin typing last name..."
                |> Ch.searchable True
                |> Ch.closeOnSelect True

        notify =
            N.init () |> N.timeout 5000 |> N.duration 500
    in
        ( State chooser False notify
        , H.send FirstList <| day3People ""
        )


update : Event -> State -> ( State, Cmd Event )
update event state =
    case event of
        Choosing select ->
            let
                ( updatedChooser, cmd ) =
                    Ch.update select state.chooser
            in
                ( { state | chooser = updatedChooser }
                , Cmd.map Choosing cmd
                )

        FirstList (Err _) ->
            let
                ( notify, cmd ) =
                    N.notify (text "Something went wrong! Try refreshing the page.") state.notify
            in
                ( { state | notify = notify }, Cmd.map Notify cmd )

        FirstList (Ok ppl) ->
            let
                listItems =
                    List.map toItem ppl
            in
                ( { state
                    | chooser = Ch.items listItems state.chooser
                  }
                , Cmd.none
                )

        SignIn ->
            if Set.isEmpty state.chooser.selected then
                let
                    ( notify, cmd ) =
                        N.notify (text "Please select your name to sign in.") state.notify
                in
                    ( { state | notify = notify }, Cmd.map Notify cmd )
            else
                ( state
                , Ch.getFirstSelected state.chooser
                    |> Maybe.withDefault "0"
                    |> String.toInt
                    |> Result.withDefault 0
                    |> signin ""
                    |> H.send Written
                )

        Written (Err _) ->
            ( state, Cmd.none )

        Written (Ok _) ->
            ( { state | isSuccessful = True }, Cmd.none )

        Notify msg ->
            let
                ( notify, cmd ) =
                    N.update msg state.notify
            in
                ( { state | notify = notify }, Cmd.map Notify cmd )


toItem : Person -> Ch.Item
toItem p =
    { label = p.fname ++ " " ++ p.lname
    , value = toString p.uuid
    , id = toString p.uuid
    }


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
        C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
            [ C.columnCenter []
                [ h3 [] [ text "Welcome to Day 3 of the AAFA Conference!" ]
                , h4 [] [ text "Please sign in below." ]
                , Html.map Choosing <| Ch.view state.chooser
                , B.model "Sign In" "primary" "medium" |> B.view SignIn
                , N.view Notify state.notify
                ]
            ]

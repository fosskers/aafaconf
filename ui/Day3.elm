module Day3 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers exposing (..)
import Ui.Button as B
import Ui.Container as C
import Http as H
import Calls exposing (..)
import Ui.Chooser as Ch
import Set


---


type Event
    = Choosing Ch.Msg
    | SignIn
    | Written (Result H.Error String)
    | FirstList (Result H.Error (List Person))


type alias State =
    { chooser : Ch.Model
    , isSuccessful : Bool
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
    in
        ( State chooser False
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
            ( state, Cmd.none )

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
                ( state, Cmd.none )
            else
                ( state
                , H.send Written <|
                    signin "" <|
                        Result.withDefault 0 <|
                            String.toInt <|
                                Maybe.withDefault "0" <|
                                    Ch.getFirstSelected state.chooser
                )

        Written (Err _) ->
            ( state, Cmd.none )

        Written (Ok _) ->
            ( { state | isSuccessful = True }, Cmd.none )


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
                ]
            ]

module Day2 exposing (..)

import Calls exposing (..)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as H
import Maybe as M
import Set
import Ui.Button as B
import Ui.Chooser as Ch
import Ui.Container as C
import Ui.Layout as L


--


type Event
    = Block Block
    | Topic String
    | Rtn (Result H.Error (List Person))
    | Choosing Ch.Msg
    | Written (Result H.Error String)
    | Submit
    | Back


type alias State =
    { blockClicked : Maybe Block
    , topicClicked : Maybe String
    , everybody : List Person
    , currentSelected : List Person
    , isSuccessful : Bool
    , chooser : Ch.Model
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
                |> Ch.multiple True
    in
        ( State Nothing Nothing [] [] False chooser, Cmd.none )


update : Event -> State -> ( State, Cmd Event )
update event state =
    case event of
        Block letter ->
            ( { state | blockClicked = Just letter }
            , H.send Rtn <| peopleFromBlock "" letter
            )

        Topic name ->
            ( { state | topicClicked = Just name }, Cmd.none )

        Rtn (Err _) ->
            ( state, Cmd.none )

        Rtn (Ok ppl) ->
            let
                listItems =
                    List.map toItem ppl
            in
                ( { state
                    | everybody = ppl
                    , chooser = Ch.items listItems state.chooser
                  }
                , Cmd.none
                )

        Choosing select ->
            let
                ( updatedChooser, cmd ) =
                    Ch.update select state.chooser
            in
                ( { state | chooser = updatedChooser }
                , Cmd.map Choosing cmd
                )

        Submit ->
            let
                block =
                    M.withDefault A state.blockClicked

                topic =
                    M.withDefault "Ass Kissing" state.topicClicked

                group =
                    state.chooser.selected
                        |> Set.map (\n -> Result.withDefault 0 <| String.toInt n)
                        |> Set.toList

                blockSignin =
                    BlockSignin block topic group
            in
                ( state, H.send Written <| groups "" blockSignin )

        Written (Err _) ->
            ( state, Cmd.none )

        Written (Ok _) ->
            ( { state | isSuccessful = True }, Cmd.none )

        Back ->
            init


toItem : Person -> Ch.Item
toItem p =
    { label = p.lname ++ "," ++ p.fname
    , value = toString p.uuid
    , id = toString p.uuid
    }


subscriptions : State -> Sub Event
subscriptions _ =
    Sub.none


centerStyle : Attribute Event
centerStyle =
    style
        [ ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "align-items", "center" )
        ]


view : State -> Html Event
view state =
    pageLayout (content state)


content : State -> Html Event
content state =
    if state.isSuccessful then
        h1 [] [ text "Success!" ]
    else
        case ( state.blockClicked, state.topicClicked ) of
            ( Nothing, Nothing ) ->
                blockPage

            ( Nothing, Just _ ) ->
                errorPage

            ( Just _, Nothing ) ->
                topicPage

            ( Just _, Just topic ) ->
                chooserPage state topic


blockPage : Html Event
blockPage =
    C.rowCenter [ style [ ( "padding-top", "15%" ) ] ]
        [ C.columnCenter []
            [ B.model "A 9AM - 10:15AM" "primary" "big" |> B.view (Block A)
            , B.model "B 10:30AM - 11:45AM" "primary" "big" |> B.view (Block B)
            , B.model "C 12PM - 1:15PM" "primary" "big" |> B.view (Block C)
            ]
        ]


errorPage : Html Event
errorPage =
    C.rowCenter [] [ text "Error", B.model "Back" "primary" "small" |> B.view Back ]


topicPage : Html Event
topicPage =
    C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
        [ C.columnCenter []
            [ B.model "Tough Decisions" "primary" "big" |> B.view (Topic "Tough Decisions")
            , B.model "Retention & Culture" "primary" "big" |> B.view (Topic "Retention & Culture")
            , B.model "Business Development" "primary" "big" |> B.view (Topic "Business Development")
            , B.model "VMS" "primary" "big" |> B.view (Topic "VMS")
            , B.model "Systems" "primary" "big" |> B.view (Topic "Systems")
            , B.model "Structures for Growth" "primary" "big" |> B.view (Topic "Structures for Growth")
            , B.model "Back" "primary" "small" |> B.view Back
            ]
        ]


chooserPage : State -> String -> Html Event
chooserPage state topic =
    C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
        [ C.columnCenter []
            [ h2 [] [ text topic ]
            , text "Please select group members from the dropdown menu"
            , Html.map Choosing <| Ch.view state.chooser
            , B.model "Submit Group" "primary" "small" |> B.view Submit
            ]
        ]

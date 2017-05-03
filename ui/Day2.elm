module Day2 exposing (..)

import Bootstrap.Button as BB
import Bootstrap.Grid as G
import Bootstrap.Grid.Col as GC
import Bootstrap.Grid.Row as GR
import Calls exposing (..)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as H
import Maybe as M
import Navigation as Nav
import Set
import Ui.Button as B
import Ui.Chooser as Ch
import Ui.Container as C
import Ui.Layout as L
import Ui.NotificationCenter as N


---


type Event
    = Block Block
    | Topic String
    | Rtn (Result H.Error (List Person))
    | Choosing Ch.Msg
    | Written (Result H.Error String)
    | Notify N.Msg
    | Submit
    | Location Nav.Location


type alias State =
    { blockClicked : Maybe Block
    , topicClicked : Maybe String
    , everybody : List Person
    , currentSelected : List Person
    , isSuccessful : Bool
    , chooser : Ch.Model
    , notify : N.Model Event
    , loc : Nav.Location
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
        chooser =
            Ch.init ()
                |> Ch.placeholder "Begin typing last name..."
                |> Ch.searchable True
                |> Ch.closeOnSelect True
                |> Ch.multiple True

        notify =
            N.init () |> N.timeout 5000 |> N.duration 500
    in
        ( State Nothing Nothing [] [] False chooser notify loc, Cmd.none )


update : Event -> State -> ( State, Cmd Event )
update event state =
    case event of
        Block letter ->
            ( { state | blockClicked = Just letter }
            , H.send Rtn <| peopleFromBlock state.loc.origin letter
            )

        Topic name ->
            ( { state | topicClicked = Just name }, Cmd.none )

        Rtn (Err _) ->
            let
                ( notify, cmd ) =
                    N.notify (text "Something went wrong! Try refreshing the page.") state.notify
            in
                ( { state | notify = notify }, Cmd.map Notify cmd )

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
            if Set.isEmpty state.chooser.selected then
                let
                    ( notify, cmd ) =
                        N.notify (text "Please select the attendees before you Submit.") state.notify
                in
                    ( { state | notify = notify }, Cmd.map Notify cmd )
            else
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
                    ( state, H.send Written <| groups state.loc.origin blockSignin )

        Written (Err _) ->
            let
                ( notify, cmd ) =
                    N.notify (text "Group sign-in failed! Please try again.") state.notify
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


toItem : Person -> Ch.Item
toItem p =
    { label = p.fname ++ " " ++ p.lname
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
        centered [ h1 [] [ text "Success! Enjoy your session." ] ]
    else
        case ( state.blockClicked, state.topicClicked ) of
            ( Nothing, Nothing ) ->
                blockPage state

            ( Nothing, Just _ ) ->
                errorPage

            ( Just block, Nothing ) ->
                topicPage state block

            ( Just _, Just topic ) ->
                chooserPage state topic


blockPage : State -> Html Event
blockPage state =
    G.container [ style [ ( "padding-top", "5%" ) ] ]
        [ G.row [ GR.centerXs ]
            [ G.col [ GC.xs8 ]
                [ BB.button [ BB.primary, BB.block, BB.attrs [ onClick <| Block A ] ]
                    [ h1 [] [ text "A" ]
                    , text "9am - 10:15am"
                    ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button [ BB.success, BB.block, BB.attrs [ onClick <| Block B ] ]
                    [ h1 [] [ text "B" ]
                    , text "10:30am - 11:45am"
                    ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button [ BB.info, BB.block, BB.attrs [ onClick <| Block C ] ]
                    [ h1 [] [ text "C" ]
                    , text "12pm - 1:15pm"
                    ]
                ]
            ]
        ]


errorPage : Html Event
errorPage =
    text "Error"


topicPage : State -> Block -> Html Event
topicPage state block =
    G.container [ style [ ( "padding-top", "5%" ) ] ]
        [ G.row [ GR.centerXs ]
            [ G.col [ GC.xs8 ] [ h1 [] [ text <| "Block " ++ toString block ] ] ]
        , G.row [ GR.centerXs ]
            [ G.col [ GC.xs8 ] [ h3 [] [ i [] [ text "Choose your session topic." ] ] ] ]
        , G.row [ GR.centerXs ]
            [ G.col [ GC.xs8 ]
                [ BB.button
                    [ BB.info, BB.block, BB.attrs [ onClick <| Topic "Tough Decisions" ] ]
                    [ text "Tough Decisions" ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button
                    [ BB.info, BB.block, BB.attrs [ onClick <| Topic "Retention & Culture" ] ]
                    [ text "Retention & Culture" ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button
                    [ BB.info, BB.block, BB.attrs [ onClick <| Topic "Business Development" ] ]
                    [ text "Business Development" ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button
                    [ BB.info, BB.block, BB.attrs [ onClick <| Topic "VMS" ] ]
                    [ text "VMS" ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button
                    [ BB.info, BB.block, BB.attrs [ onClick <| Topic "Systems" ] ]
                    [ text "Systems" ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button
                    [ BB.info, BB.block, BB.attrs [ onClick <| Topic "Structures for Growth" ] ]
                    [ text "Structures for Growth" ]
                ]
            ]
        ]


chooserPage : State -> String -> Html Event
chooserPage state topic =
    C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
        [ C.columnCenter []
            [ h2 [] [ text topic ]
            , i [] [ text "Please select group members from the dropdown menu." ]
            , Html.map Choosing <| Ch.view state.chooser
            , B.model "Submit Group" "primary" "medium" |> B.view Submit
            , N.view Notify state.notify
            ]
        ]

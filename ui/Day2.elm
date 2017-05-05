module Day2 exposing (..)

import Bootstrap.Button as BB
import Bootstrap.Form.Input as BI
import Bootstrap.Grid as G
import Bootstrap.Grid.Col as GC
import Bootstrap.Grid.Row as GR
import Calls exposing (..)
import Dict
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as H
import Navigation as Nav
import Ui.NotificationCenter as N


---


type Event
    = Block Block
    | Topic String
    | Rtn (Result H.Error (List Person))
    | Written (Result H.Error String)
    | Notify N.Msg
    | Typed String
    | Chosen Person
    | Cancelled Person
    | Submit
    | Location Nav.Location


type alias State =
    { blockClicked : Maybe Block
    , topicClicked : Maybe String
    , everybody : List Person
    , selected : Dict.Dict Int Person
    , typed : String
    , isSuccessful : Bool
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
        notify =
            N.init () |> N.timeout 5000 |> N.duration 500
    in
        ( State Nothing Nothing [] Dict.empty "" False notify loc, Cmd.none )


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
            ( { state | everybody = ppl }, Cmd.none )

        Submit ->
            if Dict.isEmpty state.selected then
                let
                    ( notify, cmd ) =
                        N.notify (text "Please select the attendees before you Submit.") state.notify
                in
                    ( { state | notify = notify }, Cmd.map Notify cmd )
            else
                let
                    block =
                        Maybe.withDefault A state.blockClicked

                    topic =
                        Maybe.withDefault "Ass Kissing" state.topicClicked

                    blockSignin =
                        BlockSignin block topic <| Dict.keys state.selected
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

        Typed s ->
            ( { state | typed = s }, Cmd.none )

        Chosen p ->
            ( { state | selected = Dict.insert p.uuid p state.selected }, Cmd.none )

        Cancelled p ->
            ( { state | selected = Dict.remove p.uuid state.selected }, Cmd.none )

        Location loc ->
            ( { state | loc = loc }, Cmd.none )


subscriptions : State -> Sub Event
subscriptions _ =
    Sub.none


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
                    , text "8:30am - 9:45am"
                    ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button [ BB.success, BB.block, BB.attrs [ onClick <| Block B ] ]
                    [ h1 [] [ text "B" ]
                    , text "10:00am - 11:15am"
                    ]
                ]
            ]
        , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
            [ G.col [ GC.xs8 ]
                [ BB.button [ BB.info, BB.block, BB.attrs [ onClick <| Block C ] ]
                    [ h1 [] [ text "C" ]
                    , text "11:30pm - 12:45pm"
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
    let
        matches =
            if String.length state.typed >= 3 then
                List.map (buttonify Chosen) <| List.filter (isMatch state) state.everybody
            else
                []
    in
        G.container [ style [ ( "padding-top", "5%" ) ] ]
            [ G.row [ GR.centerXs ]
                [ G.col []
                    [ h2 [] [ text topic ], N.view Notify state.notify ]
                ]
            , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                [ G.col []
                    [ i [] [ text "Please type the name of a group member, and select them as matches appear." ] ]
                ]
            , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                [ G.col []
                    [ BI.text [ BI.id "lname", BI.onInput Typed ] ]
                ]
            , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                [ G.col [] matches ]
            , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                [ G.col []
                    [ h4 [] [ text "Your group:" ]
                    , i [] [ text "You can reclick someone to remove them." ]
                    ]
                ]
            , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                [ G.col [] <| List.map (buttonify Cancelled) <| Dict.values state.selected ]
            , G.row [ GR.centerXs, GR.attrs [ style [ ( "padding-top", "10px" ) ] ] ]
                [ G.col [ GC.xs6 ]
                    [ BB.button [ BB.primary, BB.block, BB.attrs [ onClick Submit ] ] [ text "Submit" ] ]
                ]
            ]


isMatch : State -> Person -> Bool
isMatch state p =
    not (Dict.member p.uuid state.selected)
        && String.contains (String.toLower state.typed) (String.toLower (p.fname ++ p.lname))


buttonify : (Person -> Event) -> Person -> Html Event
buttonify f p =
    BB.button [ BB.info, BB.attrs [ onClick <| f p ] ] [ text <| p.fname ++ " " ++ p.lname ]

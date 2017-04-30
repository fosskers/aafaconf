module Day1 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers exposing (..)
import Ui.Button as B
import Ui.Container as C
import Random as R
import Http as H
import Calls exposing (..)


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
        , subscriptions = subscriptions
        }


init : ( State, Cmd Event )
init =
    ( State "" "" "" "" False, Cmd.none )



-- Need to account for empty fields being submitted
-- Need to add max string length to inputs!


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


subscriptions : State -> Sub Event
subscriptions _ =
    Sub.none


view : State -> Html Event
view state =
    pageLayout <| formLayout state


formLayout : State -> Html Event
formLayout state =
    if state.isSuccessful then
        C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
            [ C.columnCenter []
                [ h1 [] [ text <| "Thanks for registering " ++ state.firstName ++ "!" ] ]
            ]
    else
        C.rowCenter [ style [ ( "padding-top", "10%" ) ] ]
            [ C.columnCenter []
                [ h3 [] [ text "Welcome to AAFA Santa Monica 2017!" ]
                , h5 [] [ text "Please register below." ]
                , div [ onInput Fname ] [ input [ placeholder "First Name" ] [] ]
                , div [ onInput Lname ] [ input [ placeholder "Last Name" ] [] ]
                , div [ onInput City ] [ input [ placeholder "City" ] [] ]
                , div [ onInput Company ] [ input [ placeholder "Company" ] [] ]
                , B.model "Submit" "primary" "medium" |> B.view Submit
                ]
            ]

module Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ui.Container as C
import Ui.Layout as L


---


headerStyle : Attribute a
headerStyle =
    style
        [ ( "background-color", "#303358" ) ]


footerStyle : Attribute a
footerStyle =
    style
        [ ( "background-color", "#db8a25" )
        , ( "padding-top", "6%" )
        ]


pageLayout : Html a -> Html a
pageLayout a =
    L.website
        [ div [ headerStyle ] [ img [ src "/assets/aafapic.png", style [ ( "padding-left", "5%" ) ] ] [] ] ]
        [ a ]
        [ div [ footerStyle ] [] ]


centered : List (Html a) -> Html a
centered hs =
    C.rowCenter [ style [ ( "padding-top", "5%" ) ] ] [ C.columnCenter [] hs ]

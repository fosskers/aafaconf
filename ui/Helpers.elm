module Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ui.Layout as L


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
        [ div [ headerStyle ] [ img [ src "aafapic.png", style [ ( "padding-left", "5%" ) ] ] [] ] ]
        [ a ]
        [ div [ footerStyle ] [] ]

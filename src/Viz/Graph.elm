module Viz.Graph where

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, style)
import Model exposing (GraphData, Node, Link)
import Json.Decode exposing (..)
import Http
import Task exposing (Task)

graphView =
    div [ id "graph"
        , style [("width", "960px"), ("height", "500px")]
        ] [ ]


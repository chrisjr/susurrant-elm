module Viz.Ordinal where

import Dict exposing (Dict)
import List
import Maybe exposing (withDefault)

type alias ColorScale comparable = Dict comparable String

colorDict : List comparable -> List String -> ColorScale comparable
colorDict xs cs = Dict.fromList <| List.map2 (,) xs cs

color' : ColorScale comparable -> comparable -> String
color' cDict c =
    Dict.get c cDict |> withDefault "#000"

category10colors : List String
category10colors =
     [ "#1f77b4"
     , "#ff7f0e"
     , "#2ca02c"
     , "#d62728"
     , "#9467bd"
     , "#8c564b"
     , "#e377c2"
     , "#7f7f7f"
     , "#bcbd22"
     , "#17becf" ]

category10 : List comparable -> ColorScale comparable
category10 domain =
    colorDict domain category10colors

cat10 : Int -> String
cat10 = color' (category10 [0..9])

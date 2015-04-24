module Viz.Bars where

import Viz.Scale exposing (..)
import Common exposing (..)
import Viz.Common exposing (..)
import Viz.Ordinal exposing (..)
import TopicData exposing (TrackInfo, TrackTopics)
import Array exposing (Array)
import List
import Maybe exposing (Maybe, withDefault)
import Html
import Text

type alias Datum =
    { x : Int
    , y : Float
    , y0 : Float
    , y1 : Float
    , trackInfo : TrackInfo
    }

colorScale : Int -> String
colorScale = color' (category10 [0..9])

toXYT : TrackTopics -> Array {x: Int, y: Float, track: TrackInfo}
toXYT {track, topics} = Array.map (\d -> {d | track = track }) topics

toData : TrackTopics -> Array Datum
toData trackTopics =
    let cumul {x, y, track} acc =
            let y0 = withDefault 0.0 <| Maybe.map .y1 <| last acc
            in Array.push { x=x, y=y, y0=y0, y1=(y0 + y), trackInfo=track} acc
    in Array.foldl cumul Array.empty (toXYT trackTopics)

events : D3.Event.Stream Datum
events = D3.Event.stream ()

barHeight : Int
barHeight = 2

bars : Scales -> D3 TrackTopics Datum
bars {xS, yS, cS} = 
  selectAll ".bar"
  |= toData >> Array.toList
     |- enter <.> append "rect"
        |. str attr "class" "bar"
        |. fun attr "width" (\d _ -> d.y |> convert yS |> toString)
        |. num attr "height" barHeight
        |. fun attr "fill" (\d _ -> d.x |> cS)
        |. D3.Event.click events (\e d _ -> d)
     |- update
        |. fun attr "x" (\d _ -> d.y0 |> convert yS |> toString)
     |- exit
        |. remove

-- data = Array.map toFloat <| Array.fromList [1..4]

barDisplay : Margins -> Float -> Float -> TrackTopics -> Html.Html
barDisplay margin w h data =
    let domains = dataDomains (Array.toList <| Array.map (.y) data.topics)
        ds = dims margin w h
    in display ds margin (bars (scales domains ds)) data

-- Html.fromElement (Text.asText (toData data))
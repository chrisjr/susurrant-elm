module Viz.Bars where

import Viz.Scale exposing (..)
import Common exposing (..)
import Viz.Common exposing (..)
import Viz.Ordinal exposing (..)
import Model exposing (TrackInfo, TrackTopics, noInfo)
import Array exposing (Array)
import List
import Maybe exposing (Maybe, withDefault)
import Html
import Graphics.Element exposing (show)
import Svg exposing (Svg, g, rect)
import Svg.Attributes as S
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

bars : Scales -> Array Datum -> List Svg
bars {xS, yS, cS} data =
    let bar d = rect [ S.width (convert yS d.y |> toString)
                     , S.height (convert xS 1.0 |> toString)
                     , S.fill (cS d.x)
                     , S.x (convert yS d.y0 |> toString)
                     ] []
    in Array.toList <| Array.map bar data

barDisplay : Margins -> Float -> Float -> TrackTopics -> Html.Html
barDisplay margin w h data =
    let domains = dataDomains (Array.toList <| Array.map (.y) data.topics)
        ds = dims margin w h
        data' = toData data
    in svgWithMargin ds margin (bars (scales domains ds) data')

exampleData =
    let f xs = { track = noInfo "", topics = mkTopics (Array.fromList xs) }
        mkTopics xs = Array.indexedMap (\i x -> {x=i, y=x}) xs
    in f [0..9]

main = barDisplay noMargin 300 5 exampleData
-- Html.fromElement (Text.asText (toData data))
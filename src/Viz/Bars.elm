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

type alias DomainFunc = Array {x: Int, y: Float} -> Domains
type alias VizFunc = Scales -> Array Datum -> List Svg

horizontalDomains : Array {x: Int, y: Float} -> Domains
horizontalDomains arr =
    { xDomain = [0, List.sum <| Array.toList <| Array.map .y arr]
    , yDomain = [0.0, 1.0]
    , cDomain = [0..9]
    }

verticalDomains : Array {x: Int, y: Float} -> Domains
verticalDomains arr =
    let [a, b] = extent (Array.toList <| Array.map .y arr)
        ys = [a - 0.1, b]
    in { xDomain = [0.0, 9.0]
       , yDomain = ys
       , cDomain = [0..9]
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
    let bar d = rect [ S.width (convert xS d.y |> toString)
                     , S.height (convert yS 1.0 |> toString)
                     , S.fill (cS d.x)
                     , S.x (convert xS d.y0 |> toString)
                     ] []
    in Array.toList <| Array.map bar data

verticalBars : Scales -> Array Datum -> List Svg
verticalBars {xS, yS, cS} data =
    let yMax = withDefault 0 <| List.head <| List.reverse yS.range
        bar d = rect [ S.width (convert xS 1.0 |> toString)
                     , S.height (convert yS d.y |> toString)
                     , S.fill (cS d.x)
                     , S.x (convert xS (toFloat d.x) |> toString)
                     , S.y (yMax - (convert yS d.y) |> toString)
                     ] []
    in Array.toList <| Array.map bar data

baseBarDisplay : DomainFunc -> VizFunc -> List Html.Attribute ->
    Margins -> Float -> Float -> TrackTopics -> Html.Html
baseBarDisplay getDomains v attrs margin w h data =
    let domains = getDomains data.topics
        ds = dims margin w h
        data' = toData data
    in svgWithMargin attrs ds margin (v (scales domains ds) data')

barDisplay : List Html.Attribute -> Margins -> Float -> Float -> TrackTopics -> Html.Html
barDisplay = baseBarDisplay horizontalDomains bars

verticalBarDisplay : List Html.Attribute -> Margins -> Float -> Float -> TrackTopics -> Html.Html
verticalBarDisplay = baseBarDisplay verticalDomains verticalBars

exampleData =
    let f xs = { track = noInfo "", topics = mkTopics (Array.fromList xs) }
        mkTopics xs = Array.indexedMap (\i x -> {x=i, y=x}) xs
    in f [0..9]

main = verticalBarDisplay [] noMargin 64 100 exampleData
-- Html.fromElement (Text.asText (toData data))
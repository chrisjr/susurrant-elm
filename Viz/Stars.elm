module Viz.Stars where

import D3 (..)
import D3.Scale
import D3.Scale (Scale, convert, linear, domain, range)
import Viz.Common (..)
import Viz.Ordinal (cat10)
import Html
import Color (Color, blue)
import List
import String

type alias Datum a number =
    { values : List number
    , id : a
    }

type alias TokenDatum = Datum String Float

type alias Scales =
    { rS : Scale Float
    , color : Color
    , opacity : Scale Float
    }

halfPi = pi * 0.5
twoPi = pi * 2.0

lineRadial0 : List (number, number) -> List (Float, Float)
lineRadial0 xs =
    let point (r, a) = let a' = a - halfPi
                       in (r * cos(a'), r * sin(a'))
    in List.map point xs

floatToStr : Float -> String
floatToStr x = if abs x < 1e-15 then "0" else toString x

lineRadial : List (number, number) -> String
lineRadial xs =
    let points = lineRadial0 xs
        pointsL = List.map (\(a, b) -> floatToStr a ++ "," ++ floatToStr b) points |> String.join "L"
    in "M" ++ pointsL ++ "Z"

addAngles : List number -> List (number, number)
addAngles xs =
    let l = List.length xs
        angle = twoPi / toFloat l
    in List.map2 (\a b -> (a, toFloat b * angle)) xs [0 .. (l-1)]

stars : Scales -> D3 (List TokenDatum) TokenDatum
stars {rS, color, opacity} = 
  selectAll ".star"
  |= identity
     |- enter <.> append "path"
        |. str attr "class" "star"
        |. str attr "fill" "none"
        |. fun attr "stroke" (\_ i -> cat10 i)
        |. fun attr "stroke-opacity" (\_ i -> convert opacity (toFloat i+1) |> toString)
--         |. D3.Event.click events (\e d _ -> d)
     |- update
        |. fun attr "d" (\d _ -> d.values
                              |> List.map (convert rS)
                              |> addAngles
                              |> lineRadial)
     |- exit
        |. remove

getDomain : List TokenDatum -> List number
getDomain = List.concatMap .values >> extent

defaultOpacity : Float -> Scale Float
defaultOpacity n = D3.Scale.log 2.0 |> domain [1.0, n] |> range [1, 0]

starDisplay : Margins -> Float -> Float -> List TokenDatum -> Html.Html
starDisplay margin w h data =
    let dataDomain = getDomain data
        rS = linear |> domain dataDomain |> range [0, w / 2.0]
        ds = dims margin w h
        stars' = stars {rS = rS, color = blue, opacity = defaultOpacity 10}
        stars'' = center w h stars'
    in display ds margin stars''  data

toData : List (List number) -> List TokenDatum
toData = List.indexedMap (\i xs -> { values = xs, id = toString i })

exampleData : List TokenDatum
exampleData = toData
    [ [3, 1, 2, 3, 4, 2]
    , [2, 3, 5, 0, 1, 2]
    ]

smallStar = starDisplay {top = 4, left = 4, right = 4, bottom = 4} 64 64

main = smallStar exampleData
module Viz.Stars where

import Common exposing (TokenDatum)
import Viz.Scale exposing (FloatScale, linear, convert)
import Viz.Common exposing (..)
import Viz.Ordinal exposing (cat10)
import Html
import Svg as S exposing (Svg, path)
import Svg.Attributes as S
import Color exposing (Color, blue)
import List
import String

type alias Scales =
    { rS : FloatScale
    , color : Color
    , opacity : FloatScale
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

star : Scales -> TokenDatum -> Svg
star {rS, color, opacity} {id, values, prob} =
    let pathStr = values
                |> List.map (convert rS)
                |> addAngles
                |> lineRadial
    in path [ S.d pathStr
            , S.fill "none"
            , S.stroke "#000"
            , S.strokeOpacity (convert opacity prob |> toString)
            ] []
 
stars : Scales -> List TokenDatum -> List Svg
stars scales lst = List.map (star scales) lst

getDomain : List TokenDatum -> List number
getDomain = List.concatMap .values >> extent

defaultOpacity : Float -> FloatScale
defaultOpacity n = linear

starDisplay : Margins -> Float -> Float -> List TokenDatum -> Html.Html
starDisplay margin w h data =
    let dataDomain = getDomain data
        rS = { linear | domain <- dataDomain, range <- [0, w / 2.0] }
        ds = dims margin w h
        stars' = stars {rS = rS, color = blue, opacity = defaultOpacity 10}
    in svgWithMargin ds margin (center w h (stars' data))

smallStar = starDisplay {top = 4, left = 4, right = 4, bottom = 4} 64 64

toData : List (List number) -> List TokenDatum
toData = List.indexedMap (\i xs -> { values = xs, id = toString i, prob = 1.0 })

exampleData : List TokenDatum
exampleData = toData
    [ [3, 1, 2, 3, 4, 2]
    , [2, 3, 5, 0, 1, 2]
    ]

main = smallStar exampleData

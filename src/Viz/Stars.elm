module Viz.Stars where

import Model exposing (TokenDatum, TokenType(..), Data)
import Viz.Scale exposing (FloatScale, linear, logScale, convert)
import Viz.Common exposing (..)
import Viz.Ordinal exposing (cat10)
import Html
import Svg as S exposing (Svg, path)
import Svg.Attributes as S
import Color exposing (Color, red, blue, green, gray, yellow)
import List
import Dict
import String

type alias Scales =
    { rS : FloatScale
    , color : String
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
floatToStr x = if abs x < 1e-10 then "0" else toString x

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
            , S.stroke color
            , S.strokeOpacity (convert opacity prob |> toString)
            ] []
 
stars : Scales -> List TokenDatum -> List Svg
stars scales lst = List.map (star scales) lst

getDomain : List TokenDatum -> List number
getDomain = List.concatMap .values >> extent

getTokenDomains : Data -> List Float
getTokenDomains data = extent <| List.concat <| Dict.values data.vocab

defaultOpacity : FloatScale
defaultOpacity = { logScale | domain <- [0.01, 1.0], range <- [0.0, 0.8] }

starDisplay : String -> List Html.Attribute -> Maybe (List Float) -> Margins -> Float -> Float -> List TokenDatum -> Html.Html
starDisplay color attrs domain margin w h data =
    let dataDomain = Maybe.withDefault (getDomain data) domain
        rS = { linear | domain <- dataDomain, range <- [0.0, w / 2.0] }
        ds = dims margin w h
        stars' = stars {rS = rS, color = color, opacity = defaultOpacity}
    in svgWithMargin attrs ds margin (center w h (stars' data))

smallStar color attrs domain =
    starDisplay color attrs domain {top = 4, left = 4, right = 4, bottom = 4} 64 64

mediumStar color attrs domain =
    starDisplay color attrs Nothing {top = 4, left = 4, right = 4, bottom = 4} 256 256

toData : List (List number) -> List TokenDatum
toData = List.indexedMap (\i xs -> { values = xs, id = toString i, tokenType = Gfcc, prob = 1.0 })

exampleData : List TokenDatum
exampleData = toData
    [ [3, 1, 2, 3, 4, 2]
    , [2, 3, 5, 0, 1, 2]
    ]

main = smallStar "#000" [] Nothing exampleData

module Viz.Common where

import Common exposing (toList)
import Mouse
import Signal
import Html exposing (Html)
import Html.Attributes as H
import Svg exposing (Svg, svg, g)
import Svg.Attributes as S
import Svg.Lazy as S
import Json.Encode exposing (string)
import Array exposing (Array)
import List
import String
import Color exposing (Color, toRgb)
import Viz.Scale exposing (FloatScale, linear)
import Viz.Ordinal exposing (..)

type alias Dimensions = { height : Float, width : Float }
type alias Margins = { top : Float, left : Float, right : Float, bottom : Float }

type alias Domains = { xDomain : List Float, yDomain: List Float, cDomain : List Int }
type alias Scales = { xS: FloatScale, yS: FloatScale, cS: Int -> String }

size   = 500
margin = { top = 10, left = 10, right = 10, bottom = 10 }

noMargin = { top = 0, left = 0, right = 0, bottom = 0 }

dims : Margins -> Float -> Float -> Dimensions
dims margin w h = { height = h - margin.top - margin.bottom
                  , width  = w - margin.left - margin.right }

scales : Domains -> Dimensions -> Scales
scales {xDomain, yDomain, cDomain} {height, width} =
    let xS = { linear | domain <- xDomain, range <- [0, width] }
        yS = { linear | domain <- yDomain, range <- [0, height] }
        cS = color' (category10 cDomain)
    in { xS = xS, yS = yS, cS = cS }

defaultDomains : Domains
defaultDomains = { xDomain = [0.0, 1.0], yDomain = [0.0, 1.0], cDomain = [0..9] }

extent : List comparable -> List comparable
extent lst =
    let min = List.minimum lst
        max = List.maximum lst
    in toList min ++ toList max

translate : number -> number -> String
translate x y = "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"

colorStr : Color -> String
colorStr c =
    let {red, green, blue, alpha} = toRgb c
        rgb = List.map toString [red, green, blue]
        val = String.join "," <| rgb ++ [toString alpha]
    in "rgba(" ++ val  ++ ")"

htmlDims : Dimensions -> Margins -> List Html.Attribute
htmlDims ds ms =
    [ H.attribute "height" (ds.height + ms.top + ms.bottom |> floor |> toString)
    , H.attribute "width" (ds.width + ms.left + ms.right |> floor |> toString)
    ]

svgWithMargin : List Html.Attribute -> Dimensions -> Margins -> List Svg -> Html
svgWithMargin attrs ds ms xs =
    svg (attrs ++ htmlDims ds ms)
        [ g [ S.transform (translate ms.left ms.top) ] xs ]

center w h xs = [ g [ S.transform (translate (w / 2.0) (h / 2.0))] xs ]

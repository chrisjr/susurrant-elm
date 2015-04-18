module Viz.Common where

import D3 (..)
import D3.Scale (..)
import Mouse
import Signal
import Html (Html, fromElement)
import Array
import Array (Array)
import List
import Graphics.Element (..)
import Viz.Ordinal (..)

type alias Dimensions = { height : Float, width : Float }
type alias Margins = { top : Float, left : Float, right : Float, bottom : Float }

type alias Domains = { xDomain : List Float, yDomain: List Float, cDomain : List Int }
type alias Scales = { xS: Scale Float, yS: Scale Float, cS: Int -> String }

size   = 500
margin = { top = 10, left = 10, right = 10, bottom = 10 }

noMargin = { top = 0, left = 0, right = 0, bottom = 0 }

dims : Margins -> Float -> Float -> Dimensions
dims margin w h = { height = h - margin.top - margin.bottom
                  , width  = w - margin.left - margin.right }

scales : Domains -> Dimensions -> Scales
scales {xDomain, yDomain, cDomain} {height, width} =
    let xS = linear |> domain xDomain |> range [0, height]
        yS = linear |> domain yDomain |> range [0, width]
        cS = color' (category10 cDomain)
    in { xS = xS, yS = yS, cS = cS }

defaultDomains : Domains
defaultDomains = { xDomain = [0.0, 1.0], yDomain = [0.0, 1.0], cDomain = [0..9] }

extent : List comparable -> List comparable
extent lst = [List.minimum lst, List.maximum lst]

dataDomains : List Float -> Domains
dataDomains lst = { xDomain = [0.0, 1.0]
                  , yDomain = [0, List.sum lst] -- extent lst
                  , cDomain = [0..9] }

translate : number -> number -> String
translate x y = "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"

svg : Dimensions -> Margins -> D3 a a
svg ds ms =
  static "svg"
  |. num attr "height" (ds.height + ms.top + ms.bottom)
  |. num attr "width"  (ds.width  + ms.left + ms.right)
  |. static "g"
     |. str attr "transform" (translate ms.left ms.top)

vis visDims margin v =
  svg visDims margin
  |. v

center w h v =
  static "g"
  |. str attr "transform" (translate (w / 2.0) (h / 2.0))
  |. v

display ds ms v data =
    let fullHeight = (ds.height + ms.top + ms.bottom)
        fullWidth = (ds.width  + ms.left + ms.right)
    in  fromElement (render fullWidth fullHeight (vis ds ms v) data)
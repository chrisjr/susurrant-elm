module Viz.Scale where

import Debug exposing (crash)

type ScaleType
    = Linear
    | Log Float

type alias Scale a b =
    { domain : List a
    , range : List b
    , scaleType : ScaleType
    }

type alias FloatScale = Scale Float Float

linear : FloatScale
linear = { domain = [0.0, 1.0], range = [0.0, 1.0], scaleType = Linear }

logScale : FloatScale
logScale = { domain = [0.0001, 1.0], range = [0.0, 1.0], scaleType = Log 2.0 }

toUnitInterval : List Float -> Float -> Float
toUnitInterval domain x =
    case domain of
      [start, end] ->
          let dist = end - start
          in (x - start) / dist

lerp : List Float -> Float -> Float
lerp range x =
    case range of
      [start, end] ->
          let dist = end - start
          in start + (dist * x)

logToLinearScale : List Float -> List Float -> Float -> FloatScale
logToLinearScale domain range base =
    { domain = List.map (logBase base) domain
    , range = range
    , scaleType = Linear 
    }

convert : FloatScale -> Float -> Float
convert {domain, range, scaleType} d =
    case scaleType of
      Linear ->
          let x = toUnitInterval domain d
          in lerp range x
      Log base ->
          let lin = logToLinearScale domain range base
          in convert lin (logBase base d)

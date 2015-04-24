module Viz where

import Viz.Common exposing (..)
import Viz.Bars exposing (bars, xScale, yScale)

main = display dims' margin (bars xScale yScale) (fromList [1.0,2.0])

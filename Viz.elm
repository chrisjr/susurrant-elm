module Viz where

import Viz.Common (..)
import Viz.Bars (bars, xScale, yScale)
import Html (Html)
import Array (Array, fromList)

main = display dims' margin (bars xScale yScale) (fromList [1.0,2.0])

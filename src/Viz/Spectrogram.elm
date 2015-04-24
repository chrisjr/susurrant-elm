module Viz.Spectrogram where

import Text
import Matrix as M
import Matrix (Matrix, Coords)

import Color (Color, grayscale)
import Graphics.Collage (..)

data : Matrix Float
data = M.initialize (50, 50) (\(i, j) -> (toFloat i) * (toFloat j))

imageMap : (Coords -> a -> Form) -> Matrix a -> List Form
imageMap f = M.flatten << M.indexedMap f

heatmap : (number -> Color) -> Matrix number -> List Form
heatmap colorScale =
    let pixSize = 1.0
        xScale i = toFloat i * pixSize
        yScale = xScale
    in imageMap (\(i,j) x -> square pixSize
                          |> filled (colorScale x)
                          |> move (xScale i, yScale j))

topCorner w h form =
    collage w h [ (form |> move (-(toFloat w)/2, -(toFloat h)/2)) ]

imshow size data =
    topCorner size size (group (heatmap grayscale (M.constrain data)))

main = imshow 500 data
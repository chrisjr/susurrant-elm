module Audio.Common where

import WebAudio exposing (..)

type Panning
    = NoPan
    | Pan Float   -- [-1, 1]


makeGain x =
    createGainNode DefaultContext
    |> tapNode .gain (setValue x)

defaultGainNode = makeGain 0.7
                |> connectNodes (getDestinationNode DefaultContext) 0 0

mulParam : Float -> AudioParam -> AudioParam
mulParam mul param = setValue (mul * getValue param) param

makeLinearPanner : Float -> PannerNode
makeLinearPanner x =
    createPannerNode DefaultContext
        |> setPanningModel EqualPower
        |> setDistanceModel Linear
        |> setPosition x 0.0 0.0

makePanner pan =
    case pan of
      Pan x -> Just <| makeLinearPanner x
      NoPan -> Nothing

maybeConnect : AudioNode a -> Maybe (AudioNode b) -> AudioNode c -> AudioNode a
maybeConnect a b c =
    case b of
      Just b' -> a |> connectNodes b' 0 0 |> connectNodes c 0 0
      Nothing -> a |> connectNodes c 0 0

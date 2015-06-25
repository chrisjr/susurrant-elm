module Audio where

import OSC exposing (Message(..))
import Model exposing (..)
import Updates exposing (..)
import TopicData exposing (topicTokens, getTokenVectors)

import Signal
import Task exposing (Task)

playTopic : Int -> Data -> Task a ()
playTopic topic data =
    let tokens = topicTokens topic data
        tokens' = List.map (\t -> (t.id, t.prob)) tokens
        update = soundUpdate ("topic" ++ toString topic) True (PlayTokens tokens')
    in Signal.send soundUpdates.address update

stopTopic : Int -> Task a ()
stopTopic topic =
    let update = soundUpdate ("topic" ++ toString topic) False StopTokens
    in Signal.send soundUpdates.address update

playTokens : String -> List (String, Float) -> Task a ()
playTokens soundID tokenProbs =
    let update = soundUpdate soundID True (PlayTokens tokenProbs)
    in Signal.send soundUpdates.address update

stopTokens : String -> Task a ()
stopTokens soundID =
    let update = soundUpdate soundID False StopTokens
    in Signal.send soundUpdates.address update

playToken : TokenDatum -> Task a ()
playToken token = playTokens token.id [(token.id, 1.0)]

stopToken : TokenDatum -> Task a ()
stopToken token = stopTokens token.id
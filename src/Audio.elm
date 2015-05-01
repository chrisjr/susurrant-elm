module Audio where

import OSC exposing (..)
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

playToken : TokenDatum -> Data -> Task a ()
playToken token data =
    let tokens = [(token.id, 1.0)]
        update = soundUpdate (token.id) True (PlayTokens tokens)
    in Signal.send soundUpdates.address update

stopToken : TokenDatum -> Task a ()
stopToken token = 
    let update = soundUpdate (token.id) False StopTokens
    in Signal.send soundUpdates.address update
module Audio where

import OSC exposing (..)
import Model exposing (..)
import TopicData exposing (topicTokens)

import Signal
import Task exposing (Task)

playTopic : Int -> Data -> Task a ()
playTopic topic data =
    let tokens = topicTokens topic data
        tokens' = List.map (\t -> (t.id, t.prob)) tokens
    in Signal.send oscOutBox.address (Just (PlayTokens tokens'))
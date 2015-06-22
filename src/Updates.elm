module Updates where

import Signal
import Debug exposing (crash)
import Dict exposing (Dict)
import Set exposing (Set)
import Task exposing (Task)
import History exposing (setPath)
import OSC exposing (Message(..))
import Audio.Granular exposing (playOffsets)
import Model exposing (Data, Probable, TokenOffset)
import Audio.Probabilities exposing (..)

actions : Signal.Mailbox (Task err ())
actions = Signal.mailbox (Task.succeed ())

toPath x = Signal.send actions.address (setPath x)

type SoundUpdate = Play String Message | Stop String Message | Noop

soundUpdates : Signal.Mailbox SoundUpdate
soundUpdates = Signal.mailbox Noop

doSoundUpdate : SoundUpdate -> Dict String Message -> Dict String Message
doSoundUpdate up dict =
    case up of
      Noop -> dict
      Play x msg -> Dict.insert x msg dict
      Stop x _ -> Dict.remove x dict


offsetFor : Data -> (String, Float) -> Maybe (Probable TokenOffset)
offsetFor data (tokenID, prob) = Dict.get tokenID (data.tokenOffsets)
                               |> Maybe.map (\[x, _] -> {offset=x, prob=prob})

getOffsetsFrom : Data -> Message -> List (Probable TokenOffset)
getOffsetsFrom data msg =
    case msg of
      PlayTokens xs ->
          List.filterMap (offsetFor data) xs
      _ ->
          []

getOffsetCDF : Data -> Dict String Message -> Maybe (CDF TokenOffset)
getOffsetCDF data msgs =
    let allOffsets = List.concatMap (getOffsetsFrom data) (Dict.values msgs)
    in case allOffsets of
         [] -> Nothing
         _ -> Just (toCDF allOffsets)

nowPlaying : Signal (Dict String Message)
nowPlaying = Signal.foldp doSoundUpdate Dict.empty soundUpdates.signal


soundUpdate : String -> Bool -> Message -> SoundUpdate
soundUpdate soundID playing msg =
    if playing then Play soundID msg else Stop soundID msg

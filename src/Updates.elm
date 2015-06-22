module Updates where

import Signal
import Debug exposing (crash)
import Set exposing (Set)
import Task exposing (Task)
import History exposing (setPath)
import OSC exposing (Message(..))
import Audio.Granular exposing (playOffsets)
import Model exposing (Probable, TokenOffset)
import Audio.Probabilities exposing (..)

actions : Signal.Mailbox (Task err ())
actions = Signal.mailbox (Task.succeed ())

toPath x = Signal.send actions.address (setPath x)

type SoundUpdate = Play String Message | Stop String Message | Noop

soundUpdates : Signal.Mailbox SoundUpdate
soundUpdates = Signal.mailbox Noop

doSoundUpdate : SoundUpdate -> Set String -> Set String
doSoundUpdate up set =
    case up of
      Noop -> set
      Play x _ -> Set.insert x set
      Stop x _ -> Set.remove x set


offsetFor : String -> Float
offsetFor _ = 0.0

getOffsetsFrom : Message -> List (Probable TokenOffset)
getOffsetsFrom msg =
    case msg of
      PlayTrack {track, startPos} ->
        crash "PlayTrack not implemented"
      SeekTrack {pos} ->
        crash "SeekTrack not implemented"
      StopTrack ->
        crash "StopTrack not implemented"
      PlayTokens xs ->
        List.map (\(a,b) -> {offset = offsetFor a, prob=b}) xs
      StopTokens ->
        []

updateOffsets : SoundUpdate -> Maybe (CDF TokenOffset) -> Maybe (CDF TokenOffset)
updateOffsets up set =
    case up of
      Noop -> set
      Play _ msg -> Just <| toCDF <| getOffsetsFrom msg
      Stop x _ -> Nothing

nowPlaying : Signal (Set String)
nowPlaying = Signal.foldp doSoundUpdate Set.empty soundUpdates.signal

activeTokenOffsets : Signal (Maybe (CDF TokenOffset))
activeTokenOffsets = Signal.foldp updateOffsets Nothing soundUpdates.signal

soundUpdate : String -> Bool -> Message -> SoundUpdate
soundUpdate soundID playing msg =
    if playing then Play soundID msg else Stop soundID msg

module Updates where

import Signal
import Set exposing (Set)
import Task exposing (Task)
import History exposing (setPath)
import OSC exposing (toOsc, Message, ExportMessage)

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

nowPlaying : Signal (Set String)
nowPlaying = Signal.foldp doSoundUpdate Set.empty soundUpdates.signal

oscMessages : Signal (Maybe ExportMessage)
oscMessages =
    let f x = case x of
                Noop -> Nothing
                Play _ m -> Just (toOsc m)
                Stop _ m -> Just (toOsc m)
    in Signal.map f soundUpdates.signal

soundUpdate : String -> Bool -> OSC.Message -> SoundUpdate
soundUpdate soundID playing msg =
    if playing then Play soundID msg else Stop soundID msg

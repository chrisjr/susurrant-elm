module Updates where

import Signal
import Task exposing (Task)
import History exposing (setPath)

actions : Signal.Mailbox (Task err ())
actions = Signal.mailbox (Task.succeed ())

toPath x = Signal.send actions.address (setPath x)
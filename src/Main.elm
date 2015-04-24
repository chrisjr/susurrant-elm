import Html exposing (Html, text)
import Signal
import String
import List
import Dict
import Task exposing (Task)
import Http
import Router exposing (Route, match, (:->))
import History exposing (setPath, path, back, forward, hash)
import Array exposing (Array)
import Common exposing (..)
import TopicData exposing (topicData)
import Model exposing (..)
import View exposing (viewOverview, viewDoc, viewTopic, wrap)

type RouteResult a
    = Page Html
    | Redirect (Task a ())

toPath : String -> RouteResult a
toPath x = Redirect <| Signal.send pathChangeMailbox.address (setPath x)

startPage _ _ _ = toPath "/index.html"

topicRoute path hash model =
    Page <| text path  -- viewTopic data state topic
trackRoute path hash model =
    Page <| text path  -- viewDoc doc data maybeTrack state
displayOverview path hash model =
    Page <| wrap <| viewOverview model defaultState

route = match
    [ "/index.html" :-> displayOverview
    , "/track" :-> trackRoute
    , "/topic" :-> topicRoute
    ] startPage

-- SIGNALS

pathChangeMailbox : Signal.Mailbox (Task error ())
pathChangeMailbox = Signal.mailbox (Task.succeed ())

port pathChanges : Signal (Task error ())
port pathChanges =
  pathChangeMailbox.signal

-- get data
trackData : Signal.Mailbox (Maybe Model.TrackData)
trackData = Signal.mailbox Nothing

port fetchTopicData : Task Http.Error ()
port fetchTopicData = TopicData.loadData `Task.andThen` TopicData.receivedData

model : Signal Model
model = Signal.map2 Model topicData.signal trackData.signal

-- Main
routed : Signal (RouteResult a)
routed = Signal.map3 route path hash model

onlyHtml : RouteResult a -> Maybe Html
onlyHtml rr =
    case rr of
      Page x -> Just x
      _ -> Nothing

onlyTasks : RouteResult a -> Maybe (Task a ())
onlyTasks rr =
    case rr of
      Redirect x -> Just x
      _ -> Nothing

port tasks : Signal (Task error ())
port tasks = Signal.filterMap onlyTasks (Task.succeed ()) routed

main = Signal.filterMap onlyHtml (text "") routed
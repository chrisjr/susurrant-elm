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
import TopicData exposing (topicData, emptyData)
import Model exposing (..)
import Updates exposing (actions, toPath)
import View exposing (viewOverview, viewDoc, viewTopic, wrap)

type RouteResult a
    = Page Html
    | Redirect (Task a ())
    | ActionPage (Task a ()) Html

routeToPath : String -> RouteResult a
routeToPath x = Redirect <| toPath x

startPage _ _ _ = routeToPath "/index.html"

fromHash : String -> State
fromHash _ = defaultState

topicRoute path hash model =
    let topic = (String.toInt <| String.dropLeft 1 path) `orElse` -1
        data = model.data `orElse` emptyData
    in Page <| wrap <| viewTopic data (fromHash hash) topic

trackRoute path hash model =
    let trackID = String.dropLeft 1 path
        data = model.data `orElse` emptyData
        track = model.track
    in ActionPage (loadTrack trackID) <| wrap <| viewDoc trackID data track (fromHash hash)

displayOverview path hash model =
    Page <| wrap <| viewOverview model (fromHash hash)

route = match
    [ "/index.html" :-> displayOverview
    , "/track" :-> trackRoute
    , "/topic" :-> topicRoute
    ] startPage

-- SIGNALS

-- get data
trackData : Signal.Mailbox (Maybe Model.TrackData)
trackData = Signal.mailbox Nothing

port fetchTopicData : Task Http.Error ()
port fetchTopicData = TopicData.loadData `Task.andThen` TopicData.receivedData

loadTrack : String -> Task Http.Error ()
loadTrack trackID =
    let trackUrl = "/data/tracks/" ++ trackID ++ ".json"
    in Http.get (TopicData.trackDataDec trackID) trackUrl `Task.andThen`
           (Signal.send trackData.address << Just)

model : Signal Model
model = Signal.map2 Model topicData.signal trackData.signal

-- Main
-- routed : Signal (RouteResult a)
routed = Signal.map3 route path hash model

onlyHtml : RouteResult a -> Maybe Html
onlyHtml rr =
    case rr of
      Page x -> Just x
      ActionPage _ x -> Just x
      _ -> Nothing

onlyTasks : RouteResult a -> Maybe (Task a ())
onlyTasks rr =
    case rr of
      Redirect x -> Just x
      ActionPage x _ -> Just x
      _ -> Nothing

port routingTasks : Signal (Task Http.Error ())
port routingTasks = Signal.filterMap onlyTasks (Task.succeed ()) routed

port runActions : Signal (Task error ())
port runActions = actions.signal

main = Signal.filterMap onlyHtml (text "") routed
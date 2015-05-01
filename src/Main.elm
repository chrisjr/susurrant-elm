import Html exposing (Html, text)
import Signal
import String
import List
import Dict
import Set exposing (Set)
import Task exposing (Task)
import Http
import Router exposing (Route, match, (:->))
import History exposing (setPath, path, back, forward, hash)
import Array exposing (Array)
import Common exposing (..)
import TopicData exposing (topicData, emptyData)
import Model exposing (..)
import Audio exposing (stopTopic)
import OSC exposing (ExportMessage)
import Updates exposing (actions, toPath, nowPlaying, oscMessages)
import View exposing (viewOverview, viewDoc, viewTopic, viewGraph, wrap)
import GraphData exposing (..)

type RouteResult a
    = Page Html
    | Redirect (Task a ())
    | ActionPage (Task a ()) Html

routeToPath : String -> RouteResult a
routeToPath x = Redirect <| toPath x

startPage _ _ _ = routeToPath "/index.html"

topicOverviewRoute path model state = Page <| text "null"

topicRoute path model state =
    let topic = (String.toInt <| String.dropLeft 1 path) `orElse` -1
        data = model.data `orElse` emptyData
    in Page <| wrap state
            <| viewTopic data state topic

trackRoute path model state =
    let trackID = String.dropLeft 1 path
        data = model.data `orElse` emptyData
        track = model.track
    in ActionPage (loadTrack trackID) <| wrap state 
           <| viewDoc trackID data track state

displayOverview path model state =
    Page <| wrap state <| viewOverview model state

graphRoute _ _ state =
    ActionPage loadGraph <| wrap state <| viewGraph

route = match
    [ "/index.html" :-> displayOverview
    , "/track" :-> trackRoute
    , "/topics" :-> topicOverviewRoute
    , "/topic" :-> topicRoute
    , "/graph" :-> graphRoute
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

loadGraph : Task Http.Error ()
loadGraph =
    Http.get graphDec "/data/graph.json" `Task.andThen` sendGraphData

model : Signal Model
model = Signal.map2 Model topicData.signal trackData.signal

state : Signal State
state = 
    let f x y z = { defaultState | oscConnected <- x, currentPath <- y, playing <- z }
    in Signal.map3 f oscConnection path nowPlaying

-- Main
-- routed : Signal (RouteResult a)
routed = Signal.map3 route path model state

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

port oscConnection : Signal Bool

port oscOut : Signal (Maybe ExportMessage)
port oscOut = oscMessages

port graphData : Signal (Maybe GraphData)
port graphData = graphRetrieve.signal

port graphClosed : Signal Bool
port graphClosed = Signal.map (\x -> x /= "/graph") path

port neighborhood : Signal (List Node)


main = Signal.filterMap onlyHtml (text "") routed
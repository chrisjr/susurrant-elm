module GraphData where

import Model exposing (Node, Link, GraphData)
import Json.Decode exposing (..)
import Task exposing (Task)

node : Decoder Node
node = string

link : Decoder Link
link =
  tuple2 (,) string string

graphDec =
    object2 (GraphData)
      ("nodes" := list node)
      ("links" := list link)

graphRetrieve : Signal.Mailbox (Maybe GraphData)
graphRetrieve = Signal.mailbox Nothing

sendGraphData : GraphData -> Task x ()
sendGraphData d =
    Signal.send graphRetrieve.address (Just d)

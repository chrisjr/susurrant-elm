module View where

import Html exposing (Html, Attribute, text, div, input, table, tbody, tr, td, hr, br, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Bootstrap.Html exposing ( container_
                               , row_
                               , colXs_
                               , glyphiconExclamationSign_)
import Maybe exposing (Maybe, withDefault, andThen)
import Viz.Bars exposing (barDisplay)
import Viz.Stars exposing (mediumStar)
import Viz.Common exposing (noMargin)
import Viz.Ordinal exposing (cat10)
import Dict
import Model exposing (Model, State)

import TopicData exposing
    (topDocsForTopic, numTopics, topicPct, topicOrder,
     trackToTokenTopics, topWordsForTopic,
     getTokenVectors, topicData)

wrap : List Html -> Html
wrap = container_

viewOverview : Model -> State -> List Html
viewOverview model state =
    let f data = List.concatMap (viewTopicOverview data state) (topicOrder data)
        output = Result.map f (model.data)
    in case output of
         Ok x -> x
         Err e -> [ text e ]

colorFor : Int -> Attribute
colorFor i = style [ ("color", cat10 i) ]

viewTopicOverview : Model.Data -> State -> Int -> List Html
viewTopicOverview data state topic =
    [ row_
      [ colXs_ 3 [ h2 [ colorFor topic ] [ text (toString topic) ] 
                 , div [] [ text (topicPct topic data) ]
                 , mediumStar ((topWordsForTopic topic data) |> getTokenVectors data)
                 ]
      , colXs_ 9 [ table [ class "table table-condensed" ]
                         [ tbody [] (List.map showBar (topDocsForTopic topic data)) ]
                 ]
      ]
    , row_ [ hr [] [] ]
    ]

trackInfo : Model.TrackInfo -> Html
trackInfo inf = text <| inf.username ++ " | " ++ inf.title

barStyle = style []
{-    style [ ("width", "500px")
          , ("height", "16px")
          ]
-}
showBar : Model.TrackTopics -> Html
showBar trackTopics =
    tr [ barStyle ]
           [ td [] [ trackInfo trackTopics.track
                   , br [] []
                   , barDisplay noMargin 500 5 trackTopics
                   ] 
           ]

showTrack : Model.Data -> Maybe Model.TrackData -> Html
showTrack data mtd =
    let trackViz = mtd `andThen` (trackToTokenTopics data >> Just) `andThen`
                   Dict.get "gfccs" `andThen`
                   (toString >> text >> Just)
                   -- (showBar >> Just)
    in withDefault (text "Display failed") trackViz

alert : List Html -> Html
alert xs =
    div [ classList [ ("alert", True), ("alert-danger", True) ] ]
        (glyphiconExclamationSign_ :: xs)

viewTopic : Model.Data -> State -> Int -> List Html
viewTopic data state topic =
    (viewTopicOverview data state topic) ++ [ alert [] ]

viewDoc : String -> Model.Data -> Maybe Model.TrackData -> State -> List Html
viewDoc doc data maybeTrack state = [
    case maybeTrack of
      Just (trackId, trackData) ->
          if trackId /= doc then alert [ text ("Fetched " ++ trackId ++
                                               "; doesn't match " ++ "doc" )]
          else alert []
      Nothing ->
          alert [text "Full data for track is missing"]
    ]

module View where

import Html exposing (Html, Attribute, text, div, input, table, tbody, tr, td, hr, br, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Html exposing ( container_
                               , row_
                               , colXs_
                               , glyphiconExclamationSign_)
import Maybe exposing (Maybe, withDefault, andThen)
import Viz.Bars exposing (barDisplay)
import Viz.Stars exposing (smallStar, mediumStar)
import Viz.Common exposing (noMargin)
import Viz.Ordinal exposing (cat10)
import Dict
import Model exposing (Model, State)
import Updates exposing (actions, toPath)

import TopicData exposing
    (topDocsForTopic, numTopics, topicPct, topicOrder,
     trackToTokenTopics, topWordsForTopic,
     getTokenVectors, topicData, topicTokens)

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
                 , mediumStar [ onClick actions.address (toPath ("/topic/" ++ toString topic)) ] 
                              (topicTokens topic data)
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
    let trackID = trackTopics.track.trackID
    in tr [ barStyle, onClick actions.address (toPath ("/track/" ++ trackID)) ]
          [ td [] [ trackInfo trackTopics.track
                  , br [] []
                  , barDisplay [] 
                               noMargin 500 5 trackTopics
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

viewTopicTokens : Model.Data -> Int -> List Html
viewTopicTokens data topic =
    let f x = div [ style [ ("float", "left"), ("margin", "4px") ] ]
                    [ smallStar [] [x]
                    , br [] []
                    , text (x.id) ]
    in List.map f (topicTokens topic data)

viewTopic : Model.Data -> Model.State -> Int -> List Html
viewTopic data state topic =
    (viewTopicOverview data state topic) ++
        viewTopicTokens data topic ++
        [ br [ style [ ("clear", "both") ] ] []
        , alert [] ]

viewDoc : String -> Model.Data -> Maybe Model.TrackData -> Model.State -> List Html
viewDoc doc data maybeTrack state = [
    case maybeTrack of
      Just (trackId, trackData) ->
          if trackId /= doc then alert [ text ("Fetched " ++ trackId ++
                                               "; doesn't match " ++ "doc" )]
          else alert []
      Nothing ->
          alert [text "Full data for track is missing"]
    ]

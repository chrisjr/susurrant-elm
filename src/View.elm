module View where

import Html exposing ( Html
                     , Attribute
                     , text
                     , div
                     , input
                     , table
                     , tbody
                     , tr
                     , td
                     , hr
                     , br
                     , small
                     , nav
                     , ul
                     , li
                     , span
                     , button
                     , h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Html exposing ( container_
                               , containerFluid_
                               , row_
                               , colXs_
                               , glyphiconExclamationSign_
                               , navbarDefault'
                               )
import Maybe exposing (Maybe, withDefault, andThen)
import Viz.Bars exposing (barDisplay, verticalBarDisplay)
import Viz.Stars exposing (smallStar, mediumStar)
import Viz.Common exposing (noMargin)
import Viz.Ordinal exposing (cat10)
import Dict
import Model exposing (Model, State)
import Common exposing (roundPct)
import Updates exposing (actions, toPath)

import TopicData exposing
    (topDocsForTopic, numTopics, topicPct, topicOrder,
     trackToTokenTopics, topWordsForTopic,
     getTokenVectors, topicData, topicTokens)

type alias HeaderLink =
    { name : String
    , titleText : String
    , path : String
    }

aLink : String -> HeaderLink -> Html
aLink current {name, titleText, path} =
    li [ classList [ ("active", current == path) ] ]
       [ Html.a [ onClick actions.address (toPath path), title titleText ] [ text name ] ]

navLinks : List HeaderLink
navLinks =
    [ HeaderLink "Overview" "Topics and Top Tracks" "/index.html" 
    , HeaderLink "Topics" "All topics at once" "/topics"
    , HeaderLink "Social Graph" "Topics in social context" "/network"
    ]

navbrand : Html
navbrand =
    Html.a [ class "navbar-brand", href "/index.html" ]
            [ text "Susurrant" ]

navheader : Html
navheader =
    div [ class "navbar-header" ]
            [ button [ class "navbar-toggle collapsed"
                     , attribute "data-toggle" "collapse"
                     , attribute "data-target" "#collapsed"
                     ]
              ([ span [ class "sr-only" ] [ text "Toggle navigation " ] ]
               ++ List.repeat 3 (span [ class "icon-bar" ] []))
            , navbrand
            ]

navbar : String -> Html
navbar currentPath =
    let links = List.map (aLink currentPath) navLinks
    in nav [ class "navbar navbar-default navbar-fixed-top" ]
           [ containerFluid_ [ navheader
                             , div [ class "collapse navbar-collapse", id "collapsed" ]
                                       [ ul [ class "nav navbar-nav" ] links ]
                             ]
           ]

wrap : String -> List Html -> Html
wrap currentPath xs = container_ <| [ navbar currentPath ] ++ xs

viewOverview : Model -> State -> List Html
viewOverview model state =
    let f data = List.concatMap (viewTopicDocOverview data state) (topicOrder data)
        output = Result.map f (model.data)
    in case output of
         Ok x -> x
         Err e -> [ text e ]

colorFor : Int -> String
colorFor i = cat10 i

colorAttrFor : Int -> Attribute
colorAttrFor i = style [ ("color", colorFor i) ]

viewTopicDocOverview : Model.Data -> State -> Int -> List Html
viewTopicDocOverview data state topic =
    [ row_
      [ div [ onClick actions.address (toPath ("/topic/" ++ toString topic))
            , class "col-xs-3 topic-overview"
            ] 
                 [ h2 [ colorAttrFor topic ] [ text ("Topic " ++ (toString topic) ++ " ")
                                             , br [] []
                                             , small [] [ (text (topicPct topic data)) ]
                                             ] 
                 , mediumStar (colorFor topic) [] (topicTokens topic data)
                 ]
      , colXs_ 9 (List.map showBar (topDocsForTopic topic data))
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
    in div [ class "row track-row"
           , onClick actions.address (toPath ("/track/" ++ trackID))
           ]
          [ colXs_ 9 [ trackInfo trackTopics.track ]
          , colXs_ 3 [ verticalBarDisplay [] noMargin 100 24 trackTopics ]
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
                    [ smallStar (colorFor topic) [] [x]
                    , br [] []
                    , text (x.id)
                    , div [ class "small" ]
                          [ text <| roundPct x.prob ]
                    ]
    in List.map f (topicTokens topic data)

viewTopic : Model.Data -> Model.State -> Int -> List Html
viewTopic data state topic =
    (viewTopicDocOverview data state topic) ++
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

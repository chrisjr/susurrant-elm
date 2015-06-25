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
                     , h2
                     , h3
                     , a
                     )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Bootstrap.Html exposing ( container_
                               , containerFluid_
                               , row_
                               , colXs_
                               , glyphiconWarningSign_
                               , glyphiconExclamationSign_
                               , glyphiconPlay_
                               , glyphiconPause_
                               , navbarDefault'
                               )
import Maybe exposing (Maybe, withDefault, andThen)
import Viz.Bars exposing (barDisplay, verticalBarDisplay)
import Viz.Stars exposing (smallStar, mediumStar, getDomain, getTokenDomains)
import Viz.Common exposing (noMargin)
import Viz.Ordinal exposing (cat10)
import Viz.Graph
import Audio exposing (..)
import Set
import Dict exposing (Dict)
import Array exposing (Array)
import Model exposing (Model, State)
import Common exposing (roundPct)
import OSC exposing (Message)
import Updates exposing (..)
import Task exposing (Task)

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
    , HeaderLink "Social Graph" "Tracks in social context" "/graph"
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

wrap : State -> List Html -> Html
wrap state xs =
    let alerts = []  -- warning [ text "OSC not connected" ] ]
    in container_ <| [ navbar (state.currentPath) ] ++ alerts ++ xs

viewOverview : Model -> State -> List Html
viewOverview = viewAllTopicsWith viewTopicDocOverview

viewAllTopics : Model -> State -> List Html
viewAllTopics = viewAllTopicsWith viewTopicOnly

viewAllTopicsWith : (Model.Data -> State -> Int -> List Html) -> Model -> State -> List Html
viewAllTopicsWith topicFunc model state =
    let f data = List.concatMap (topicFunc data state) (topicOrder data)
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
         (viewTopicOnly data state topic ++
          [ colXs_ 9 (List.map showBar (topDocsForTopic topic data)) ])
    , row_ [ hr [] [] ]
    ]

viewTopicOnly : Model.Data -> State -> Int -> List Html
viewTopicOnly data state topic =
    let tokenDomains = getTokenDomains data
        starPlot = mediumStar (colorFor topic) [ attribute "class" "center-block" ] (Just tokenDomains) (topicTokens topic data)
    in [ div [ onClick actions.address (toPath ("/topic/" ++ toString topic))
                  , onMouseEnter actions.address (playTopic topic data)
                  , onMouseLeave actions.address (stopTopic topic)
                  , class "col-xs-3 topic-overview"
                  ] 
              [ h2 [ colorAttrFor topic ] [ text ("Topic " ++ (toString topic) ++ " ")
                                          , br [] []
                                          , small [] [ (text (topicPct topic data)) ]
                                          ] 
              , starPlot
              ]
            ]

trackInfoFmt : Model.TrackInfo -> Html
trackInfoFmt inf = text <| inf.username ++ " | " ++ inf.title

showBar : Model.TrackTopics -> Html
showBar trackTopics =
    let trackID = trackTopics.track.trackID
    in div [ class "row track-row"
           , onClick actions.address (toPath ("/track/" ++ trackID))
           ]
          [ colXs_ 9 [ trackInfoFmt trackTopics.track ]
          , colXs_ 3 [ verticalBarDisplay [] noMargin 100 24 trackTopics ]
          ]
          

showTrack : Model.Data -> Maybe Model.TrackData -> Html
showTrack data mtd =
    let trackViz = mtd `andThen` (trackToTokenTopics data >> Just) `andThen`
                   Dict.get "gfccs" `andThen`
                   (toString >> text >> Just)
                   -- (showBar >> Just)
    in withDefault (text "Display failed") trackViz


warning : List Html -> Html
warning = alertBase False

alert : List Html -> Html
alert = alertBase True

alertBase b xs =
    let icon = if b then glyphiconExclamationSign_ else glyphiconWarningSign_
    in div [ classList [ ("alert", True)
                       , ("alert-danger", b)
                       , ("alert-warning", not b) ] ]
           (icon :: text " " :: xs)

viewTopicTokens : Model.Data -> Int -> List Html
viewTopicTokens data topic =
    let tokens = topicTokens topic data
        tokenDomains = getDomain tokens
        playPause x = [ onMouseEnter actions.address (playToken x)
                      , onMouseLeave actions.address (stopToken x)
                      ]
        f x = div ([ style [ ("display", "inline-block"), ("margin", "4px") ] ] ++ playPause x)
                    [ smallStar (colorFor topic) [] (Just tokenDomains) [x]
                    , br [] []
                    , text (x.id)
                    , div [ class "small" ]
                          [ text <| roundPct x.prob ]
                    ]
    in List.map f tokens

viewTopic : Model.Data -> Model.State -> Int -> List Html
viewTopic data state topic =
    (viewTopicDocOverview data state topic) ++
        viewTopicTokens data topic ++
        [ br [ style [ ("clear", "both") ] ] []
        , alert [] ]

viewDoc : String -> Model.Data -> Maybe Model.TrackData -> Model.State -> List Html
viewDoc doc data maybeTrack state =
    case maybeTrack of
      Just (trackId, trackData) ->
          if trackId /= doc then [ text "Loading..." ]
          else viewDocData state data (TopicData.trackInfo data trackId) trackData
      Nothing ->
          [ text "Loading..." ]


viewDocData : Model.State -> Model.Data
                          -> Model.TrackInfo
                          -> Model.TrackTokens
                          -> List Html
viewDocData state data info tokens =
    let info' = trackInfoFmt info
        byDtypes = TopicData.tokensByDtype data (info.trackID) tokens
        topicDict = TopicData.tokensToTopics data byDtypes
    in [ h3 [] [ a [ href info.url ] [ info' ] ]
       , viewDocTopicBar state info "gfccs" byDtypes topicDict
       , viewDocTopicBar state info "beat_coefs" byDtypes topicDict
       , viewDocTopicBar state info "chroma" byDtypes topicDict
       ]


viewDocTopicBar : Model.State -> Model.TrackInfo
                              -> String
                              -> Dict String (Array Int)
                              -> Dict String (Array Int)
                              -> Html
viewDocTopicBar state info dtype byDtypes topicDict =
    let topics = Dict.get dtype topicDict
                 |> withDefault (Array.empty)
                 |> Array.filter (\x -> x /= -1)
        trackTopics = { track = info, topics = mkTopics topics }
        mkTopics xs = Array.map (\x -> {x=x, y=1.0}) xs
        (action, playIcon') = mkPlayIcon dtype info byDtypes state
        isEmpty = Array.isEmpty topics
    in if (not isEmpty) then
           div [ action ] [ colXs_ 2 [ text dtype ]
                  , colXs_ 1 [ playIcon' ]
                  , colXs_ 9 [ barDisplay [] noMargin 500 20 trackTopics ]
                  ]
           else div [] []

mkPlayIcon : String
    -> Model.TrackInfo
    -> Dict String (Array Int)
    -> Model.State
    -> (Html.Attribute, Html)
mkPlayIcon dtype info byDtypes state =
    let soundId = info.trackID ++ "/" ++ dtype
        tokenProbs = TopicData.tokensToProbDist dtype byDtypes
        playAct = playTokens soundId tokenProbs
        stopAct = stopTokens soundId
    in playIcon soundId playAct stopAct state

playIcon : String -> Task a () -> Task a () -> Model.State -> (Html.Attribute, Html)
playIcon soundId playAct stopAct state =
    let isPlaying = soundId `Set.member` state.playing
        icon = if isPlaying then glyphiconPause_ else glyphiconPlay_
        action = if isPlaying
                 then onClick actions.address stopAct
                 else onClick actions.address playAct
    in (action, div [ ] [ icon ])
    

viewGraph : Model -> Model.State -> List Html
viewGraph model state =
    [ graphKey
    , Viz.Graph.graphView
    , div [] [ text <| toString state.neighborhood ]
    ]

pxStr : Int -> String
pxStr px = (toString px) ++ "px"

valign = ("vertical-align", "middle")

swatch : String -> String -> Html
swatch pxs hex =
    div [ style [ ("display", "inline-block")
                , ("margin", "0 4px")
                , valign
                , ("background", hex)
                , ("width", pxs)
                , ("height", pxs) ]
        ] []

legendLine : Int -> String -> String -> Html
legendLine size hex label =
    let pxs = pxStr size
    in div [ style [ ("line-height", pxs)
                   , ("height", pxs)
                   , valign
                   , ("margin-bottom", "4px")
                   ]
           ]
           [ swatch pxs hex
           , span [ style [ valign ] ] [ text label ]
           ]

graphKey : Html
graphKey =
    div [] [ legendLine 24 "#009EE8" "User"
           , legendLine 24 "#FF6117" "Track" 
           ]
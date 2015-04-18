import Graphics.Input as Input
import Html (Html, Attribute, text, div, input, h2)
import Html.Attributes (..)
import Html.Events (on, targetValue)
import Bootstrap.Html (container', row', colXs', glyphiconExclamationSign')
import Maybe
import Maybe (Maybe(..), withDefault, andThen)
import Signal
import String
import List
import Dict
import Array (Array)
import D3.Event
import Viz.Bars
import Viz.Bars (barDisplay)
import Viz.Stars (smallStar)
import Viz.Common (noMargin)
import Viz.Ordinal (cat10)
import Common (..)
import TopicData
import TopicData (topDocsForTopic, numTopics, topicPct, topicOrder,
                  TrackTokens, trackToTokenTopics)

type Mode
    = Overview
    | TopicFocus Int
    | DocFocus String

type Action
    = NoOp
    | ToOverview
    | ToTopic Int
    | ToDoc String

type alias State =
    { mode : Mode }

view : Result String TopicData.Data
    -> Maybe TopicData.TrackData
    -> State
    -> Html
view maybeData maybeTrack state =
    case maybeData of
      Ok data ->
          container' ([ row' [ colXs' 12 [ showTrack data maybeTrack ] ] ] ++
                    case state.mode of
                        Overview ->
                            viewOverview data state
                        TopicFocus topic ->
                            viewTopic data state topic
                        DocFocus doc -> viewDoc doc data maybeTrack state
                     )
      Err x -> container' [ row' [ text x ] ]

viewOverview : TopicData.Data -> State -> List Html
viewOverview data state =
    List.concatMap (viewTopicOverview data state) (topicOrder data)

colorFor : Int -> Attribute
colorFor i = style [ ("color", cat10 i) ]

viewTopicOverview : TopicData.Data -> State -> Int -> List Html
viewTopicOverview data state topic =
    [ row'
      [ colXs' 3 [ h2 [ colorFor topic ] [ text (toString topic) ] 
                 , div [] [ text (topicPct topic data) ]
                 , smallStar ((topWordsForTopic topic data) |> getWords)
                 ]
      , colXs' 9 <| List.map showBar
                 <| topDocsForTopic topic data
      ]
    ]

showBar : TopicData.TrackTopics -> Html
showBar trackTopics = barDisplay noMargin 500 15 trackTopics

showTrack : TopicData.Data -> Maybe TopicData.TrackData -> Html
showTrack data mtd =
    let trackViz = mtd `andThen` (trackToTokenTopics data >> Just) `andThen`
                   Dict.get "gfccs" `andThen`
                   (toString >> text >> Just)
                   -- (showBar >> Just)
    in withDefault (text "Display failed") trackViz

alert : List Html -> Html
alert xs =
    div [ classList [ ("alert", True), ("alert-danger", True) ] ]
        (glyphiconExclamationSign' :: xs)

viewTopic : TopicData.Data -> State -> Int -> List Html
viewTopic data state topic =
    (viewTopicOverview data state topic) ++ [ alert [] ]

viewDoc : String -> TopicData.Data -> Maybe TopicData.TrackData -> State -> List Html
viewDoc doc data maybeTrack state = [
    case maybeTrack of
      Just (trackId, trackData) ->
          if trackId /= doc then alert [ text ("Fetched " ++ trackId ++
                                               "; doesn't match " ++ "doc" )]
          else alert []
      Nothing ->
          alert [text "Full data for track is missing"]
    ]


-- SIGNALS

-- main : Signal Html
main = Signal.map3 view TopicData.loadedData trackData appState

handler : Viz.Bars.Datum -> Maybe TopicData.TrackInfo -> Maybe TopicData.TrackInfo
handler d _ = Just (d.trackInfo)

d3Events : Signal (Maybe TopicData.TrackInfo)
d3Events = D3.Event.folde handler Nothing Viz.Bars.events

appState : Signal State
appState = Signal.foldp step initialState (Signal.subscribe updates)

initialState : State
initialState = { mode = Overview }

step : Action -> State -> State
step action oldState = 
    case action of
      NoOp       -> oldState
      ToOverview -> { oldState | mode <- Overview }
      ToTopic a  -> { oldState | mode <- TopicFocus a }
      ToDoc a    -> { oldState | mode <- DocFocus a }

updates : Signal.Channel Action
updates =
    Signal.channel NoOp

port trackData : Signal (Maybe TopicData.TrackData)

port requestTrack : Signal (Maybe String)
port requestTrack =
    Signal.map (Maybe.map (.trackID)) d3Events
{-
port requestTrack =
    let getDocName {mode} = case mode of
                              DocFocus doc -> Just doc
                              _ -> Nothing                       
    in Signal.map getDocName appState
-}
module Test where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)
import ElmTest.Runner.Element exposing (runDisplay)

import Debug exposing (crash)
import List
import Result
import Json.Decode exposing (Decoder, dict, list, float)
import Test.Fixtures exposing (..)

import Common exposing (..)
import Viz.Scale exposing (..)
import Viz.Stars
import TopicData

type Trial = Success | Failure String

toTrial : Result String value -> Trial
toTrial x =
    case x of
      Ok _ -> Success
      Err e -> Failure e

isOk : Result String value -> Assertion
isOk = toTrial >> assertEqual Success

unsafeGetOk : Result error value -> value
unsafeGetOk x =
    case x of
      Ok v -> v
      Err e -> crash ("Not OK: " ++ toString e) 

-- Viz.Scale

lerpTests : List Test
lerpTests =
    [ lerp [0.0, 1.0] 0.5 `equals` 0.5
    , lerp [-2.0, 5.0] 0.0 `equals` -2.0
    , lerp [-1.0, 1.0] 0.75 `equals` 0.5 ]

minMax : FloatScale -> Test
minMax scale =
    let r1 = Maybe.map (convert scale) (List.head scale.domain)
        r2 = Maybe.map (convert scale) (List.head <| List.reverse scale.domain)
    in (toList r1 ++ toList r2) `equals` scale.range

suiteVizScale =
    Suite "Viz.Scale" <|
          lerpTests ++ [ minMax linear
                       , minMax { linear | domain <- [-200, 500] }
                       , minMax { linear | range <- [-5.0, -3.0] }
                       ]

-- Viz.Stars

radialPoints = [(10, 0), (20, 1), (20, 2), (10, 3)]
radialResult = "M0,-10L16.82941969615793,-10.806046117362794L18.185948536513635,8.32293673094285L1.4112000805986715,9.899924966004454Z"

suiteVizStars =
    Suite "Viz.Stars"
              [ radialResult `equals` Viz.Stars.lineRadial radialPoints ]

-- TopicData

assertDec : Decoder a -> String -> Assertion
assertDec dec s = Json.Decode.decodeString dec s |> isOk

assertTopicDist : String -> Assertion
assertTopicDist = assertDec TopicData.topicDist

jsonTests : List Test
jsonTests =
    [ test "topic_tokens" (assertDec TopicData.topicTokenDec topic_tokens_json)
    , test "doc_topics" (assertTopicDist doc_topics_json)
    , test "token_topics" (assertTopicDist token_topics_json)
    , test "doc_metadata" (assertDec (dict TopicData.trackInfoDec)
                                     doc_metadata_json)
    , test "token_topics" (assertTopicDist token_topics_json)
    , test "vocab" (assertDec (dict (list float)) vocab_json)
    ]

topicFixtureData : Result String TopicData.Data
topicFixtureData = TopicData.fromResults <|
    List.map Ok [ topics_json
                , doc_topics_json
                , token_topics_json
                , topic_tokens_json
                , doc_metadata_json
                , vocab_json
                ]

getTopWordsFrom = Result.map (TopicData.topWordsForTopic 0)

topWordsTest : Test
topWordsTest =
    let topWords = getTopWordsFrom topicFixtureData
        lenTopWords = Result.map List.length topWords
    in test "topWordsForTopic" (assertEqual (Ok 10) lenTopWords)

topWordVectorsTest : Test
topWordVectorsTest =
    let topWords = getTopWordsFrom topicFixtureData
        tokens = Result.map2 TopicData.getTokenVectors topicFixtureData topWords
    in test "getTokenVectors" (assertEqual (Ok 10) (Result.map List.length tokens))

suiteTopicData =
    Suite "TopicData" <| jsonTests ++ [topWordsTest, topWordVectorsTest]

allTests = Suite "App" [ suiteVizStars
                       , suiteVizScale
                       , suiteTopicData
                       ]

main = runDisplay allTests
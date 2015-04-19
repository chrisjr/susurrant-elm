module Test where

import ElmTest.Assertion (..)
import ElmTest.Test (..)
import ElmTest.Runner.Element (runDisplay)

import Debug (crash)
import List
import Result
import Json.Decode
import Test.Fixtures (..)


import Viz.Stars 
import TopicData


isOkBool : Result error value -> Bool
isOkBool x =
    case x of
      Ok _ -> True
      Err _ -> False

isOk : Result error value -> Assertion
isOk = isOkBool >> assert

unsafeGetOk : Result error value -> value
unsafeGetOk x =
    case x of
      Ok v -> v
      Err e -> crash ("Not OK: " ++ toString e) 

-- Viz.Stars

radialPoints = [(10, 0), (20, 1), (20, 2), (10, 3)]
radialResult = "M0,-10L16.82941969615793,-10.806046117362794L18.185948536513635,8.32293673094285L1.4112000805986715,9.899924966004454Z"

starTests : List Test
starTests = [ radialResult `equals` Viz.Stars.lineRadial radialPoints ]

-- TopicData

assertDec : Json.Decode.Decoder a -> String -> Assertion
assertDec dec s = Json.Decode.decodeString dec s |> isOk

assertTopicDist : String -> Assertion
assertTopicDist = assertDec TopicData.topicDist

jsonTests : List Test
jsonTests =
    [ test "topic_tokens" (assertDec TopicData.topicTokenDec topic_tokens_json)
    , test "doc_topics" (assertTopicDist doc_topics_json)
    , test "token_topics" (assertTopicDist token_topics_json)
    , test "doc_metadata" (assertDec
                           (Json.Decode.dict TopicData.trackInfoDec)
                           doc_metadata_json)
    , test "token_topics" (assertTopicDist token_topics_json)
    ]

getTopicFixtureData : Result String TopicData.Data
getTopicFixtureData = TopicData.fromResults <|
    List.map Ok [ topics_json
                , doc_topics_json
                , token_topics_json
                , topic_tokens_json
                , doc_metadata_json
                ]

topWordsTest : Test
topWordsTest =
    let data = getTopicFixtureData
        topWords = Result.map (TopicData.topWordsForTopic 0) data
        lenTopWords = Result.map List.length topWords
    in test "topWordsForTopic" (assertEqual (Ok 10) lenTopWords)

topicDataTests : List Test
topicDataTests = jsonTests ++ [topWordsTest]

suiteVizStars = Suite "Viz.Stars" starTests
suiteTopicData = Suite "TopicData" topicDataTests

allTests = Suite "App" [ suiteVizStars
                       , suiteTopicData
                       ]

main = runDisplay allTests
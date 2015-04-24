module TopicData where

import Array exposing (Array)
import Debug exposing (crash)
import Dict exposing (Dict)
import Http
import Task exposing (Task, andMap)
import Signal exposing (..)
import String
import Result
import Maybe exposing (Maybe, withDefault)
import List exposing (sortBy, (::))
import Set exposing (Set)
import Json.Decode exposing ( Decoder
                            , (:=)
                            , decodeString
                            , object3
                            , int
                            , string
                            , dict
                            , array
                            , list
                            , float
                            , at
                            , keyValuePairs)
import Common exposing (..)
import Model exposing (..)
-- import Viz.Stars exposing (TokenDatum)

numTopics : Data -> Int
numTopics = .topicPrevalence >> Array.length

emptyData : Data
emptyData = Data Array.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty

trackInfoDec : Decoder TrackInfo
trackInfoDec =
    object3 (TrackInfo)
      (Json.Decode.map toString <| "id" := int)
      ("title" := string)
      (at ["user", "username"] string)          

topicDist : Decoder (Dict String (Array Float))
topicDist = dict (array float)

unsafeToInt : String -> Int
unsafeToInt s =
    case (String.toInt s) of
      Ok i -> i

toIntDict : Dict String a -> Dict Int a
toIntDict = Dict.toList
            >> List.map (\(a, b) -> (unsafeToInt a, b))
            >> Dict.fromList

topicTokenDec : Decoder (Dict Int (List (String, Float)))
topicTokenDec = dict (keyValuePairs float) |> Json.Decode.map toIntDict

thresh : Int -> Float -> String -> Array Float -> Bool
thresh topic min _ arr = (nth topic arr) > min

trackToTokenTopics : Data -> TrackData -> Dict String TrackTopics
trackToTokenTopics data (track, trackTokens) =
    let topicDict = tokensToTopics data track trackTokens
        info = trackInfo data track
        f xs = Array.map (\i -> {x=i, y=1.0/toFloat (Array.length xs)}) xs
    in Dict.map (\_ v -> { track = info, topics = f v}) topicDict

tokenTopic : Data -> String -> Int -> Int
tokenTopic data dtype dnum =
    let tokName = dtype ++ (toString dnum)
        topic = Dict.get tokName (data.tokenTopics)
    in withDefault -1 <| Maybe.map argmax topic

tokensToTagged : TrackTokens -> Array TaggedToken
tokensToTagged tokens =
    Array.map (\(a, b, c) -> TaggedToken a b c) tokens

tokensToTopics : Data -> String -> TrackTokens -> Dict String (Array Int)
tokensToTopics data track allTokens =
    let getTopic = tokenTopic data
        tagged = tokensToTagged allTokens
        ofType getter = Array.map getter tagged
        get dtype getter = (dtype, Array.map (getTopic dtype) (ofType getter))
    in Dict.fromList [ get "gfccs" .gfcc
                     , get "beat_coefs" (withDefault -1 << .beat_coef)
                     , get "chroma" .chroma]

getTopicsForDoc : Data -> String -> Result String (Array Float)
getTopicsForDoc data doc =
    Result.fromMaybe ("Doc " ++ doc ++ "not found") (Dict.get doc (data.docTopics))

toXY : Array Float -> Array {x: Int, y: Float}
toXY = Array.indexedMap (\i y -> {x=i, y=y})

topDocsForTopic : Int -> Data -> List TrackTopics
topDocsForTopic topic data =
    let aboveThresh = Dict.toList <| Dict.filter (thresh topic 0.1) (data.docTopics)
        getInfo t = trackInfo data (segToTrackId t)
        docs = List.map (\(k,v) -> {track=getInfo k, topics=toXY v}) aboveThresh
        topDocs = List.reverse <| sortBy (nth topic << Array.map (.y) << .topics) docs
    in  List.take 10 topDocs

topWordsForTopic : Int -> Data -> List (String, Float)
topWordsForTopic topic data =
    let topWords = Dict.get topic (data.topicTokens)
    in List.take 10 (withDefault [] topWords)

getVector : Data -> (String, Float) -> Maybe TokenDatum
getVector data (token, prob) =
    let vec = Dict.get token (data.vocab)
        f v = { values = v, id = token, prob = prob }
    in Maybe.map f vec

getTokenVectors : Data -> List (String, Float) -> List TokenDatum
getTokenVectors data tokens = List.filterMap (getVector data) tokens

noInfo : String -> TrackInfo
noInfo track = { trackID = track, title = "", username = "" }

segToTrackId : String -> String
segToTrackId seg =
    let parts = String.split "." seg
    in withDefault "" <| List.head parts

trackInfo : Data -> String -> TrackInfo
trackInfo data track =
    withDefault (noInfo track) <| Dict.get track (data.docMetadata)

topicPct : Int -> Data -> String
topicPct i data =
    let amt = withDefault 0.0 <| Array.get i (data.topicPrevalence)
        pct = toString <| amt * 100.0
    in  String.left 4 pct ++ "%"

topicOrder : Data -> List Int
topicOrder data =
    let f a = withDefault 0.0 <| Array.get a (data.topicPrevalence)
    in List.reverse <| sortBy f [0.. (numTopics data) - 1]

updateOrFail : (a -> Data -> Data) -> Decoder a -> Result String String -> Data -> Result String Data
updateOrFail update dec resp data =
    let decoded = resp `Result.andThen` decodeString dec
    in case decoded of
      Ok x -> Ok (update x data)
      Err e -> Err e

addTopicPrevalence a data = { data | topicPrevalence <- a }
addDocTopics a data = { data | docTopics <- a }
addTokenTopics a data = { data | tokenTopics <- a }
addTopicTokens a data = { data | topicTokens <- a }
addDocMetadata a data = { data | docMetadata <- a }
addVocab a data = { data | vocab <- a }

fromResults : List (Result String String) -> Result String Data
fromResults results =
    let updates = 
            [ updateOrFail addTopicPrevalence (array float)
            , updateOrFail addDocTopics topicDist
            , updateOrFail addTokenTopics topicDist
            , updateOrFail addTopicTokens topicTokenDec
            , updateOrFail addDocMetadata (dict trackInfoDec)
            , updateOrFail addVocab (dict (list float))
            ]
        updates' = List.map2 (|>) results updates
    in List.foldl (flip Result.andThen) (Ok emptyData) updates'

prefix : String
prefix = "/data/"

loadData : Task Http.Error Data
loadData =
    Task.map Data (Http.get (array float) (prefix ++ "topics.json"))
            `andMap` Http.get topicDist (prefix ++ "doc_topics.json")
            `andMap` Http.get topicDist (prefix ++ "token_topics.json")
            `andMap` Http.get topicTokenDec (prefix ++ "topic_tokens.json")
            `andMap` Http.get (dict trackInfoDec) (prefix ++ "doc_metadata.json")
            `andMap` Http.get (dict (list float)) (prefix ++ "vocab.json")

topicData : Signal.Mailbox (Result String Data)
topicData = Signal.mailbox (Err "Loading...")

receivedData : Data -> Task x ()
receivedData data = Signal.send topicData.address (Ok data)
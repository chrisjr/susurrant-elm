module Model where

import Array exposing (Array)
import Dict exposing (Dict)
import Debug exposing (crash)
import Json.Decode exposing (Decoder)
import Set exposing (Set)
import String exposing (startsWith)

type alias Model =
    { data : Result String Data
    , track : Maybe TrackData
    }

type TokenType
    = Gfcc
    | BeatCoef
    | Chroma
    | Text

type alias TokenDatum =
    { values : List Float
    , id : String
    , tokenType : TokenType
    , prob : Float
    }

type alias Data =
    { topicPrevalence : Array Float
    , docTopics : Dict String (Array Float)
    , tokenTopics : Dict String (Array Float)
    , topicTokens : Dict Int (List (String, Float))
    , docMetadata : Dict String TrackInfo
    , vocab : Dict String (List Float)
    , tokenMaxLikelyTopic : Dict String Int
    }

type alias TrackInfo =
    { trackID : String
    , title : String
    , username : String
    , url : String
    }

type alias TrackToken = (Maybe Int, Int, Int)

type alias TaggedToken =
    { beat_coef : Maybe Int
    , chroma : Int
    , gfcc : Int
    }

type alias TrackTokens = Array TrackToken
type alias TrackData = (String, TrackTokens)
type alias TrackTopics =
    { track : TrackInfo
    , topics: Array {x: Int, y: Float}
    }

type alias Node = String

type alias Link = (String, String)

type alias GraphData =
    { nodes : List Node
    , links : List Link
    }

type Mode
    = Overview
    | TopicFocus Int
    | DocFocus String

type alias State = 
    { mode : Mode
    , currentPath : String
    , oscConnected : Bool
    , playing : Set String
    }

defaultState : State
defaultState = { mode = Overview
               , oscConnected = False
               , currentPath = "/index.html"
               , playing = Set.empty
               }

noInfo : String -> TrackInfo
noInfo track = { trackID = track, title = "", username = "", url = "" }

tokenTypeOf : String -> TokenType
tokenTypeOf x =
    if | startsWith "gfcc" x -> Gfcc
       | startsWith "chroma" x -> Chroma
       | startsWith "beat_coef" x -> BeatCoef
       | otherwise -> Text
module Model where

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder)

type alias Model =
    { data : Result String Data
    , track : Maybe TrackData
    }

type alias TokenDatum =
    { values : List Float
    , id : String
    , prob : Float
    }

type alias Data =
    { topicPrevalence : Array Float
    , docTopics : Dict String (Array Float)
    , tokenTopics : Dict String (Array Float)
    , topicTokens : Dict Int (List (String, Float))
    , docMetadata : Dict String TrackInfo
    , vocab : Dict String (List Float)
    }

type alias TrackInfo =
    { trackID : String
    , title : String
    , username : String
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

type Mode
    = Overview
    | TopicFocus Int
    | DocFocus String

type alias State = 
    { mode : Mode
    }

defaultState : State
defaultState = { mode = Overview }

noInfo : String -> TrackInfo
noInfo track = { trackID = track, title = "", username = "" }

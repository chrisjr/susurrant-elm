module Audio.Granular where

import Audio.Probabilities exposing (..)
import Debug
import Html exposing (Html, text)
import Maybe exposing (withDefault)
import Model exposing (TokenOffset)
import Random exposing (Seed, Generator, generate, list, pair, float)
import Signal exposing ((<~), (~))
import Task exposing (Task)
import Time exposing (Time, millisecond, every)
import WebAudio exposing (..)

type alias Envelope =
    { attack : Float
    , release : Float
    }

type Panning
    = NoPan
    | Pan Float   -- [-1, 1]

type alias GrainParams =
    { transposition : Float
    , pan : Panning
    , bufferOffset : Float
    , triggerAt : Float
    , amp : Float
    , envelope : Envelope
    }

type alias Model = { buffer : Maybe AudioBuffer
                   , envelope : Envelope
                   , seed : Seed
                   , bufferSpread : Float
                   , triggerSpread : Float
                   , offsets : Maybe (CDF TokenOffset)
                   , volume : Float
                   }

type Action
    = NoOp
    | NewEnv Envelope
    | NewSeed Seed
    | BufferChange (Maybe AudioBuffer)
    | PlayOffsets (Maybe (CDF TokenOffset))

defaultEnvelope : Envelope
defaultEnvelope = { attack = 0.4, release = 0.4 }

defaultModel : Model
defaultModel = { buffer = Nothing
               , envelope = defaultEnvelope
               , seed = Random.initialSeed 0
               , bufferSpread = 0.5
               , triggerSpread = 0.2
               , offsets = Nothing
               , volume = 0.7 }

defaultGrainParams : GrainParams
defaultGrainParams =
    { transposition = 1.0
    , pan = NoPan
    , bufferOffset = 0.0
    , triggerAt = 0.0
    , amp = 0.8
    , envelope = defaultEnvelope
    }

makeGain x =
    createGainNode DefaultContext
    |> tapNode .gain (setValue x)

defaultGainNode = makeGain 0.7
                |> connectNodes (getDestinationNode DefaultContext) 0 0

mulParam : Float -> AudioParam -> AudioParam
mulParam mul param = setValue (mul * getValue param) param

totalTime {attack, release} = attack + release

isJust : Maybe a -> Bool
isJust x = case x of
             Just _ -> True
             Nothing -> False

makeLinearPanner : Float -> PannerNode
makeLinearPanner x =
    createPannerNode DefaultContext
        |> setPanningModel EqualPower
        |> setDistanceModel Linear
        |> setPosition x 0.0 0.0

makePanner pan =
    case pan of
      Pan x -> Just <| makeLinearPanner x
      NoPan -> Nothing

maybeConnect : AudioNode a -> Maybe (AudioNode b) -> AudioNode c -> AudioNode a
maybeConnect a b c =
    case b of
      Just b' -> a |> connectNodes b' 0 0 |> connectNodes c 0 0
      Nothing -> a |> connectNodes c 0 0

startSourceAndEnv source localGain envelope bufferOffset triggerAt amp =
    let dur = totalTime envelope
        start = triggerAt + getCurrentTime DefaultContext
        source' = source
                |> startAudioBufferNode start bufferOffset (Just dur)
                |> stopAudioBufferNode (start + dur + 0.1)
        localGain' = localGain
                   |> tapNode .gain (linearRampToValue amp (start + envelope.attack))
                   |> tapNode .gain (linearRampToValue 0.0 (start + dur))
    in localGain

-- makeGrain : AudioBuffer -> GrainParams -> AudioNode a
makeGrain buffer {transposition, pan, amp, bufferOffset, triggerAt, envelope} =
    let localGain = makeGain 0.0
        panner = makePanner pan
        dur = totalTime envelope
        source = createAudioBufferSourceNode DefaultContext
               |> setAudioBufferForNode buffer
               |> tapNode .playbackRate (mulParam transposition)
               |> connectNodes localGain 0 0
        _ = maybeConnect localGain panner defaultGainNode
    in startSourceAndEnv source localGain envelope bufferOffset triggerAt amp

makeParams : Model -> List Float -> GrainParams
makeParams model [baseOffset, x, y, z, w] =
    let pan = if w < 0.5 then Pan ((z * 2.0) - 1.0) else NoPan
        bufferOffset = baseOffset + (model.bufferSpread * x)
    in { defaultGrainParams | bufferOffset <- bufferOffset,
                              triggerAt <- y * model.triggerSpread,
                              pan <- pan,
                              amp <- model.volume * defaultGrainParams.amp,
                              envelope <- model.envelope }

randomParams n = list n (list 4 (float 0 1))

untilLength : Int -> List a -> List a
untilLength n = List.take n << List.concat << List.repeat n

makeRandomParams voices model = 
    let (params, seed') = generate (randomParams voices) model.seed
        (offsets, seed'') = case model.offsets of
                    Just cdf -> generate (list voices (sampleOffsets cdf)) seed'
                    Nothing -> ([], seed')
    in (List.map2 (::) offsets params, seed'')

triggerGrains : Int -> Model -> Task x ()
triggerGrains voices model =
    case model.buffer of
      Just buffer' ->
           let (offsets, seed') = makeRandomParams voices model
               grainsWith = makeGrain buffer' << makeParams model
               _ = List.map grainsWith offsets
           in Signal.send actions.address (NewSeed seed')
      Nothing -> Task.succeed ()

-- Updates
update : Action -> Model -> Model
update action model =
    case action of
      NewSeed s -> { model | seed <- s }
      NewEnv e -> { model | envelope <- e }
      BufferChange a -> { model | buffer <- a }
      PlayOffsets l -> { model | offsets <- l }
      NoOp -> model

view = text << toString

doAudio model =
    triggerGrains 8 model

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

-- Signals

audioBuffer : Signal (Maybe AudioBuffer)
audioBuffer = loadAudioBufferFromUrl DefaultContext "/data/samples.wav"

bufferLoaded : Signal (Task x ())
bufferLoaded =
    let f x = Signal.send actions.address (BufferChange x)
    in Signal.map f audioBuffer

model : Signal Model
model = Signal.foldp update defaultModel actions.signal

grainTrigger : Signal Time
grainTrigger = every (100 * millisecond)

audioTasks : Signal (Task x ())
audioTasks = model
           |> Signal.sampleOn grainTrigger
           |> Signal.map doAudio

playOffsets : Maybe (CDF TokenOffset) -> Task x ()
playOffsets = Signal.send actions.address << PlayOffsets

{-
port bufferDone : Signal (Task x ())
port bufferDone = bufferLoaded

port audioDone : Signal (Task x ())
port audioDone = audioTasks

main = view <~ model
-}
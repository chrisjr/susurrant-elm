module Audio.Granular where

import Audio.Common exposing (..)
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
                   , gainNodes : List GainNode
                   }

type Action
    = NoOp
    | NewEnv Envelope
    | NewSeed Seed
    | BufferChange (Maybe AudioBuffer)
    | PlayOffsets (Maybe (CDF TokenOffset))

defaultEnvelope : Envelope
defaultEnvelope = { attack = 0.2, release = 0.2 }

defaultModel : Model
defaultModel = { buffer = Nothing
               , envelope = defaultEnvelope
               , seed = Random.initialSeed 0
               , bufferSpread = 0.3
               , triggerSpread = 0.2
               , offsets = Nothing
               , gainNodes = List.map (\_ -> makeGain 0.0) [1..numVoices]
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

totalTime {attack, release} = attack + release

isJust : Maybe a -> Bool
isJust x = case x of
             Just _ -> True
             Nothing -> False

startSourceAndEnv source localGain envelope bufferOffset triggerAt amp =
    let dur = totalTime envelope
        start = triggerAt + getCurrentTime DefaultContext
        source' = source
                |> startAudioBufferNode start bufferOffset (Just dur)
                |> stopAudioBufferNode (start + dur + 0.1)
        localGain' = localGain
                   |> tapNode .gain (linearRampToValue amp (start + envelope.attack))
                   |> tapNode .gain (linearRampToValue 0.0 (start + dur))
    in localGain'

-- makeGrain : AudioBuffer -> GrainParams -> AudioNode a
makeGrain localGain buffer {transposition, pan, amp, bufferOffset, triggerAt, envelope} =
    let panner = makePanner pan
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
               grainsWith gain off = makeGrain gain buffer' <| makeParams model off
               _ = List.map2 grainsWith model.gainNodes offsets
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

view = text << toString << .offsets

numVoices = 8

doAudio model =
    triggerGrains numVoices model

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

-- Signals

audioBuffer : Task err ()
audioBuffer = loadAudioBufferFromUrl DefaultContext "/data/samples.mp3" `Task.andThen`
              \x -> Signal.send actions.address (BufferChange (Just x))

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
port bufferLoad : Task x ()
port bufferLoad = audioBuffer

port audioDone : Signal (Task x ())
port audioDone = audioTasks

majorProbs = List.map (\x -> { offset = x * 1.0, prob = 1.0/3.0}) [0.0, 4.0, 7.0]
major = toCDF majorProbs

port startMajor : Task x ()
port startMajor = playOffsets (Just major)

main = view <~ model
-}
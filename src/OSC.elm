module OSC where

import Signal exposing ((<~), (~))
import Json.Decode exposing (Decoder)
import Html exposing (Html, text, button, div)
import Html.Events exposing (onClick)

type Message
    = PlayTrack { track : String
                , startPos : Float 
                }
    | SeekTrack { pos : Float }
    | StopTrack
    | PlayTokens (List (String, Float))
    | StopTokens

type alias ExportMessage = (String, List (List String, List Float))

toOsc : Message -> ExportMessage
toOsc m = case m of
    PlayTrack {track, startPos} ->
        ("/track/play", [([track], [startPos])])
    SeekTrack {pos} ->
        ("/track/seek", [([], [pos])])    
    StopTrack ->
        ("/track/stop", [])
    PlayTokens xs ->
        ("/tokens/play", List.map (\(a,b) -> ([a], [b])) xs)
    StopTokens ->
        ("/tokens/stop", [])

{--

view : Bool -> Html
view connected =
    div []
        [ text (if connected then "Connected" else "Not connected")
        , button [ onClick oscOutBox.address (Just (PlayTrack {track = "x", startPos= 0.2})) ]
                     [ text "Play Track" ]
        , button [ onClick oscOutBox.address (Just StopTrack) ]
                     [ text "Stop Track" ]
        , button [ onClick oscOutBox.address (Just (PlayTokens [("a", 0.5), ("b", 0.2)])) ]
                 [ text "Play tokens" ]
        ]


-- SIGNALS
oscOutBox : Signal.Mailbox (Maybe Message)
oscOutBox = Signal.mailbox Nothing

port oscConnection : Signal Bool

port oscOut : Signal (Maybe ExportMessage)
port oscOut = Signal.map (Maybe.map toOsc) oscOutBox.signal

--MAIN

main = view <~ oscConnection
--}

module OSC where

import Signal exposing ((<~), (~))
import Json.Decode exposing (Decoder)
import Html exposing (Html, text, button)
import Html.Events exposing (onClick)

type Message
    = PlayTrack { track : String
                , startPos : Float 
                }
    | StopTrack
    | PlayToken { token : String
                , vol : Float
                , pan : Float
                }
    | StopToken { token : String }

type alias ExportMessage = (String, List String, List Float)

toOsc : Message -> ExportMessage
toOsc m = case m of
    PlayTrack {track, startPos} ->
        ("/track/play", [track], [startPos])
    StopTrack ->
        ("/track/stop", [], [])
    PlayToken {token, vol, pan} ->
        ("/token/play", [token], [vol, pan])
    StopToken {token} ->
        ("/token/stop", [token], [])

view : Bool -> Html
view _ =
    button [ onClick oscOutBox.address (Just StopTrack) ]
        [ text "Stop Track" ]


-- SIGNALS
oscOutBox : Signal.Mailbox (Maybe Message)
oscOutBox = Signal.mailbox Nothing

{-
port oscConnected : Signal Bool

port oscOut : Signal (Maybe ExportMessage)
port oscOut = Signal.map (Maybe.map toOsc) oscOutBox.signal

--MAIN

main = view <~ oscConnected
-}

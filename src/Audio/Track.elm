module Audio.Track where

audioBuffer : Signal (Maybe AudioBuffer)
audioBuffer = loadAudioBufferFromUrl DefaultContext "/data/samples.mp3"

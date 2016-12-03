#I @"C:\Users\chris\Source\Repos\SAPP2\src\SappLib\Scripts"
#load "load-project-debug.fsx"

open Player
open Config

let mf = MusicFile "x"
let s (MusicFile x) = x

let config = 
    { SAPExePath = "C:\\_data\\SAP\\StealthAudioPlayer.exe"
      SOXExePath = ""
      WAVCachePath = ""
      ConvertArguments = ""
      ConvertHDArguments = ""
      FLACRootPath = "" }

let playList = PlayList([ "C:\\_data\\Hifi\\WAVs\\Satin_Doll.wav" ] |> List.map MusicFile)

let callback d = 
    printf "\nReceived callback: "
    match d with
    | Playing(_, TrackIndex t) -> printf "Playing index %i" t
    | Playing(_, NoTrack) -> printf "Not really"
    | Loaded _ -> printf "Loaded ..."
    | Starting _ -> printf "Starting ..."
    | Stopping _ -> printf "Stopping ..."
    | _ -> printf "Some other state"
    printf "\n"

let api = Player.createApi config callback

api.handle (SetPlayList playList)
api.handle Play
api.handle Stop


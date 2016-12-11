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

let playList = PlayList([ "C:\\_data\\Hifi\\WAVs\\Satin_Doll.wav"; "C:\\_data\\Hifi\\WAVs\\Satin_Doll2.wav" ] |> List.map MusicFile)

let callback d = 
    printf "\nReceived callback: "
    match d with
    | Playing(_, TrackIndex t, elapsed) -> printf "Playing index %i elapsed %O" t elapsed
    | Loaded _ -> printf "Loaded ..."
    | Starting _ -> printf "Starting ..."
    | Stopping _ -> printf "Stopping ..."
    | GoingToNext _ -> printf "Going to next ..."
    | GoingToPrevious _ -> printf "Going to previous ..."
    | Paused _ -> printf "Paused ..."
    | Pausing _ -> printf "Pausing ..."
    | Broken reason -> printf "Broken: %s" reason
    | NoDisk -> printf "No disk ..."
    | _ -> printf "Some other state"
    printf "\n"

let api = Player.createApi config
api.changes |> Observable.subscribe callback

api.handle (SetPlayList playList)
api.handle Play
api.handle Next
api.handle Previous
api.handle Stop
api.handle Pause

Player.isLastTrack playList (TrackIndex 0)




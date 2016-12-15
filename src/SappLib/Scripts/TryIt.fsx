#I @"C:\Users\chris\Source\Repos\SAPP2\src\SappLib\Scripts"
#load "load-project-debug.fsx"

open DriveImpl
open DriveModel
open Config
open Microsoft.FSharp.Reflection

let config = {Config.empty with SAPExePath = "C:\\_data\\SAP\\StealthAudioPlayer.exe" }

let playList = PlayList([ "C:\\_data\\Hifi\\WAVs\\Satin_Doll.wav"; "C:\\_data\\Hifi\\WAVs\\Satin_Doll2.wav" ] |> List.map MusicFile)
let playList2 = PlayList([ "C:\\_data\\Hifi\\WAVs\\short1.wav"; "C:\\_data\\Hifi\\WAVs\\short2.wav"; "C:\\_data\\Hifi\\WAVs\\short3.wav" ] |> List.map MusicFile)

let getUnionCaseName (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  

let printCommands cmds = 
    cmds
    |> List.map getUnionCaseName
    |> String.concat ", "

let callback d = 
    printf "\nReceived callback: "
    match d with
    | Playing(_, TrackIndex t, elapsed) -> printf "Playing index %i elapsed %O" t elapsed
    | _ -> printf "%s ..." (getUnionCaseName d)
    d |> DriveImpl.availableCommands |> printCommands |> printf "\nAvailable commands: %s\n"

let api = DriveImpl.createApi config
api.changes |> Observable.subscribe callback

api.handle (SetPlayList playList2)
api.handle Play
api.handle Next
api.handle Previous
api.handle Stop
api.handle Pause
api.handle Query





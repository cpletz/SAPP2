#I @"C:\Users\chris\Source\Repos\SAPP2\src\SappLib\Scripts"
#load "load-project-debug.fsx"

open Config
open Converter
open DriveModel
open System.IO

let config = 
    { Config.empty with SOXExePath = "C:\\_data\\SAP\\Codecs\\sox\\sox.exe"
                        SAPExePath = "C:\\_data\\SAP\\StealthAudioPlayer.exe" }

let wavBasePath = "C:\\_data\\Hifi\\WAVs\\"
let flacFiles = Directory.GetFiles("C:\\_data\\Hifi\\FLACs\\Trichotomy\\The Gentle War", "*.flac")

let filesToConvert = 
    FilesToConvert(flacFiles
                   |> Array.mapi (fun i flac -> 
                          { Id = MusicFileId(i.ToString())
                            WavFile = WavPath(sprintf "%s%i.wav" wavBasePath i)
                            FlacFile = FlacPath(flac)
                            SamplingRate = 96000 })
                   |> Array.toList)

let conversionList = 
    match filesToConvert with
    | FilesToConvert(l) -> l

let playList = PlayList(conversionList |> List.map ((fun { WavFile = WavPath(f) } -> f) >> MusicFile))

let converterCallback s = 
    printf "\nReceived converter callback: "
    match s with
    | Idle -> printf "Idle"
    | Cancelling -> printf "Cancelling"
    | Failed({ FlacFile = FlacPath(file) }, reason) -> printf "Conversion of file '%s' failed: %s" file reason
    | Converting(_, todo) -> printf "Converting ... %i to do" todo.Length

let printDriveCommands cmds = 
    cmds
    |> List.map Utils.getUnionCaseName
    |> String.concat ", "

let driveCallback d = 
    printf "\nReceived drive callback: "
    match d with
    | Playing(_, TrackIndex t, elapsed) -> printf "Playing index %i elapsed %O" t elapsed
    | _ -> printf "%s ..." (Utils.getUnionCaseName d)
    d
    |> Drive.availableCommands
    |> printDriveCommands
    |> printf "\nAvailable commands: %s\n"

let drive = Drive.createApi config

drive.changes |> Observable.subscribe driveCallback

let converter = Converter.createApi config
converter.progress |> Observable.subscribe converterCallback
converter.execute (ConvertFiles filesToConvert)
converter.execute Cancel

converter.execute ConverterCommand.Query

drive.execute (SetPlayList playList)
drive.execute Play

drive.execute Stop

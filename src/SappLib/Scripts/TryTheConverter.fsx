#I @"C:\Users\chris\Source\Repos\SAPP2\src\SappLib\Scripts"
#load "load-project-debug.fsx"

open Config
open Converter
open System.IO

let config = { Config.empty with SOXExePath = "C:\\_data\\SAP\\Codecs\\sox\\sox.exe" }
let wavBasePath = "C:\\_data\\Hifi\\WAVs\\"
let flacFiles = Directory.GetFiles("C:\\_data\\Hifi\\FLACs\\Trichotomy\\Fact Finding Mission", "*.flac")

let filesToConvert = 
    FilesToConvert(flacFiles
                   |> Array.mapi (fun i flac -> 
                          { Id = MusicFileId(i.ToString())
                            WavFile = WavPath(sprintf "%s%i.wav" wavBasePath i)
                            FlacFile = FlacPath(flac)
                            SamplingRate = 96000 })
                   |> Array.toList)

let callback s = 
    printf "\nReceived callback: "
    match s with
    | Idle -> printf "Idle"
    | Cancelling -> printf "Cancelling"
    | Failed({ FlacFile = FlacPath(file) }, reason) -> printf "Conversion of file '%s' failed: %s" file reason
    | Converting(_, todo) -> printf "Converting ... %i to do" todo.Length

let api = Converter.createApi config

api.progress |> Observable.subscribe callback

api.execute (ConvertFiles filesToConvert)
api.execute Cancel


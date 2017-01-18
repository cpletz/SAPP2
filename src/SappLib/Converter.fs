module Converter

open Config
open Microsoft.Win32.SafeHandles
open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks

type MusicFileId = 
    | MusicFileId of string

type FlacPath = 
    | FlacPath of string

type WavPath = 
    | WavPath of string

type FileToConvert = 
    { Id : MusicFileId
      FlacFile : FlacPath
      WavFile : WavPath
      SamplingRate: int }

type FilesToConvert = 
    | FilesToConvert of FileToConvert list

type ConverterCommand = 
    | ConvertFiles of FilesToConvert
    | Cancel
    | Query

type ConverterState = 
    | Idle
    | Converting of files : FilesToConvert * toDo : FileToConvert list
    | Cancelling
    | Failed of FileToConvert * reason : string

type ConverterApi = 
    { execute : ConverterCommand -> unit
      progress : IObservable<ConverterState> }

let rec private lowerSamplingRate current target =
    if current <= target then current
    else lowerSamplingRate (current/2) target

let rec private hightenSamplingRate current target =
    if (current * 2) > target then current
    else hightenSamplingRate (current * 2) target

let private getConversionArgs config file = 
    let targetSamplingRate =
        if file.SamplingRate >= config.TargetSamplingRate then
            lowerSamplingRate file.SamplingRate config.TargetSamplingRate
        else 
            hightenSamplingRate file.SamplingRate config.TargetSamplingRate

    let {WavFile = WavPath(wavFile); FlacFile = FlacPath(flacFile)} = file
            
    sprintf "\"%s\" −b %i \"%s\" rate %i" flacFile config.TargetBitRate wavFile targetSamplingRate

let private deleteWavFile (WavPath file) = File.Delete file

let private convertFile config file (cancellationToken : CancellationToken) success error = 
    let info = ProcessStartInfo(config.SOXExePath, (getConversionArgs config file))
    info.UseShellExecute <- false
    info.CreateNoWindow <- true
    use soxProcess = new Process()
    soxProcess.StartInfo <- info
    if soxProcess.Start() then 
        use processSafeHandle = new SafeWaitHandle(soxProcess.Handle, false)
        use processHandle = new AutoResetEvent false
        processHandle.SafeWaitHandle <- processSafeHandle
        let waitHandles = 
            [| processHandle :> WaitHandle
               cancellationToken.WaitHandle |]
        WaitHandle.WaitAny waitHandles |> ignore
        if cancellationToken.IsCancellationRequested then 
            if not soxProcess.HasExited then 
                soxProcess.Kill()
                soxProcess.WaitForExit()
            deleteWavFile file.WavFile
        else 
            if soxProcess.ExitCode <> 0 then 
                let errMsg = soxProcess.StandardError.ReadToEnd()
                error file errMsg
        success file
    else 
        let reason = sprintf "Could not start %s" config.SOXExePath
        error file reason

let private startFileConversion config file (cancellationToken : CancellationToken) success error = 
    Task.Factory.StartNew(fun _ -> convertFile config file cancellationToken success error) |> ignore

let private convertFiles config (FilesToConvert(files)) (cancellationToken : CancellationToken) success error = 
    let noOfParallelConversions = 4
    
    let convertNow, convertLater = 
        if files.Length > noOfParallelConversions then files |> List.splitAt noOfParallelConversions
        else files, []
    convertNow |> List.iter (fun f -> startFileConversion config f cancellationToken success error)
    convertNow, convertLater

type private ConversionActorCommand = 
    | ExternalCommand of ConverterCommand
    | FileConverted of FileToConvert
    | ConversionFailed of FileToConvert * reason : string

type private ConversionActorState = 
    { State : ConverterState
      InProgress : FileToConvert list
      NextConversion : FilesToConvert option
      CTS : CancellationTokenSource option }

let private idleState = 
    { State = Idle
      InProgress = []
      NextConversion = None
      CTS = None }

let private progressEvents = Event<ConverterState>()

let createApi (config : ConfigData) = 
    let conversionActor = 
        MailboxProcessor.Start(fun inbox -> 
            let onConversionSucceeded file = inbox.Post(FileConverted file)
            let onConversionFailed file reason = inbox.Post(ConversionFailed(file, reason))
            
            let rec messageLoop loopState = 
                async { 
                    let! conversionCmd = inbox.Receive()
                    let newState = 
                        match conversionCmd, loopState with
                        // New conversion request ...
                        // ...the converter is idle
                        | ExternalCommand(ConvertFiles(files)), { State = Idle } -> 
                            let cts = new CancellationTokenSource()
                            let inPogress, stillToDo = convertFiles config files cts.Token onConversionSucceeded onConversionFailed
                            { State = Converting(files, stillToDo)
                              InProgress = inPogress
                              NextConversion = None
                              CTS = Some(cts) }
                        // New conversion request ...
                        // ...the converter is converting
                        | ExternalCommand(ConvertFiles(files)), { State = Converting(_); InProgress = inProgress; CTS = Some(cts) } -> 
                            cts.Cancel()
                            { State = Cancelling
                              InProgress = inProgress
                              NextConversion = Some(files)
                              CTS = Some(cts) }
                        // New conversion request ...
                        // ...the converter is cancelling
                        | ExternalCommand(ConvertFiles(files)), { State = Cancelling; InProgress = inProgress; CTS = Some(cts) } -> 
                            { State = Cancelling
                              InProgress = inProgress
                              NextConversion = Some(files)
                              CTS = Some(cts) }
                        // New cancellation request ...
                        // ...the converter is converting
                        | ExternalCommand(Cancel(_)), { State = Converting(_); InProgress = inProgress; CTS = Some(cts) } -> 
                            cts.Cancel()
                            { State = Cancelling
                              InProgress = inProgress
                              NextConversion = loopState.NextConversion
                              CTS = Some(cts) }
                        // The conversion of a file has finished ...
                        // ...the converter is converting
                        | FileConverted(file), { State = Converting(files, todo); InProgress = inProgress; CTS = Some(cts) } -> 
                            match todo with
                            | [] -> 
                                cts.Dispose()
                                progressEvents.Trigger(Converting(files, []))
                                idleState
                            | next :: rest -> 
                                startFileConversion config next cts.Token onConversionSucceeded onConversionFailed
                                { State = Converting(files, rest)
                                  InProgress = inProgress |> Utils.removeFromList file
                                  NextConversion = None
                                  CTS = Some(cts) }
                        // The conversion of a file has finished ...
                        // ...the converter is cancelling
                        | FileConverted(file), { State = Cancelling(_); InProgress = inProgress; CTS = Some(cts) } -> 
                            let newInProgress = inProgress |> Utils.removeFromList file
                            match newInProgress with
                            | [] -> 
                                cts.Dispose()
                                if loopState.NextConversion.IsSome then 
                                    inbox.Post (ExternalCommand(ConvertFiles(loopState.NextConversion.Value)))
                                idleState
                            | _ -> { loopState with InProgress = newInProgress}
                        // The conversion of a file has failed ...
                        // ...the converter is converting
                        | ConversionFailed(file, reason), { State = Converting(_); InProgress = inProgress; CTS = Some(cts) } -> 
                            cts.Cancel()
                            progressEvents.Trigger(Failed(file, reason))
                            { State = Cancelling
                              InProgress = inProgress |> Utils.removeFromList file
                              NextConversion = None
                              CTS = Some(cts) }                        
                        // The cancellation of a conversion has failed ...
                        // ...the converter is cancelling
                        | ConversionFailed(file, _), { State = Cancelling; InProgress = inProgress; CTS = Some(cts) }  -> 
                            match inProgress with
                            | [] -> 
                                if loopState.NextConversion.IsSome then 
                                    inbox.Post (ExternalCommand(ConvertFiles(loopState.NextConversion.Value)))
                                idleState
                            | _ -> { loopState with InProgress = inProgress |> Utils.removeFromList file}


                        | _ -> loopState
                    progressEvents.Trigger newState.State
                    return! messageLoop newState
                }
            messageLoop idleState)
    
    let exec conversionCmd = conversionActor.Post(ExternalCommand conversionCmd)
    { execute = exec
      progress = progressEvents.Publish }

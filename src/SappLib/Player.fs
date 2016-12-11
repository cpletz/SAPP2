module Player

open System
open System.IO
open System.Text
open System.Diagnostics
open Config
open System.Threading

type Result<'a> = 
    | Success of 'a
    | Failure of string

type MusicFile = 
    | MusicFile of string

type PlayList = 
    | PlayList of MusicFile list

type CurrentTrack = 
    | TrackIndex of int

type Drive = 
    | NoDisk
    | Loaded of playList : PlayList
    | Playing of playList : PlayList * track : CurrentTrack * elapsed : TimeSpan
    | Paused of playList : PlayList * track : CurrentTrack
    | Starting of playList : PlayList
    | Stopping of playList : PlayList
    | Pausing of playList : PlayList * track : CurrentTrack
    | Resuming of playList : PlayList * track : CurrentTrack
    | GoingToNext of playList : PlayList * track : CurrentTrack
    | GoingToPrevious of playList : PlayList * track : CurrentTrack
    | Broken of reason : string

type Command = 
    | SetPlayList of PlayList
    | Play
    | Stop
    | Pause
    | Next
    | Previous
    | Query

type Api = 
    { handle : Command -> unit
      changes : IObservable<Drive> }

type InternalCommand = 
    | ExternalCommand of Command
    | PlayListChange
    | NextAction of (Drive -> Drive)

let private encoding = Encoding.GetEncoding("ISO-8859-1")

let private writePlayListFile (config : ConfigData) (PlayList playList) = 
    use fs = new FileStream(config.SAPPlaylistPath, FileMode.Create, FileAccess.Write)
    use writer = new StreamWriter(fs, encoding)
    playList |> Seq.iter (fun (MusicFile mf) -> writer.WriteLine mf)

let rec openPlayListFile (config : ConfigData) retryCnt = 
    if retryCnt > 0 then 
        let lines = 
            try 
                Some(File.ReadAllLines(config.SAPPlaylistPath, encoding))
            with _ -> None
        match lines with
        | Some(content) -> Success content
        | None -> 
            Thread.Sleep 100
            openPlayListFile config (retryCnt - 1)
    else Failure "Cannot open play-list file."

let private findCurrentIndex config = 
    match (openPlayListFile config 10) with
    | Success(playListContent) -> 
        let index = playListContent |> Array.tryFindIndex (fun l -> l.StartsWith "#")
        match index with
        | Some(i) -> Success(TrackIndex i)
        | None -> Success(TrackIndex 0)
    | Failure(msg) -> Failure(sprintf "Could nor retrieve current track (Reason: %s)" msg)

let private getElapsedTime (config : ConfigData) = 
    let playListFileInfo = FileInfo config.SAPPlaylistPath
    DateTime.Now.Subtract(playListFileInfo.LastWriteTime)

let private handlePlayerProcessExited e = ()

let private prepareSapProcess config = 
    let info = ProcessStartInfo config.SAPExePath
    info.WorkingDirectory <- config.SAPDirectory
    info.UseShellExecute <- false
    let sapProcess = new Process()
    sapProcess.StartInfo <- info
    (info, sapProcess)

let private startSapProcess config = 
    let _, sapProcess = prepareSapProcess config
    sapProcess.EnableRaisingEvents <- true
    sapProcess.Exited.Add handlePlayerProcessExited
    sapProcess.Start() |> ignore

let private runSapControlProcess config command = 
    let info, p = prepareSapProcess config
    use sapProcess = p
    info.Arguments <- command
    sapProcess.Start() |> ignore
    sapProcess.WaitForExit()

let isLastTrack (PlayList(playList)) (TrackIndex(index)) = 
    playList
    |> List.length
    |> (=) (index + 1)

let private isFirstTrack (TrackIndex(index)) = index = 0

let private setPlayList drive playList = 
    match drive with
    | NoDisk -> Loaded playList
    | _ -> drive

let private play config drive = 
    match drive with
    | Loaded(playList) -> 
        writePlayListFile config playList
        startSapProcess config
        Starting playList
    | Paused(playList, _) -> 
        startSapProcess config
        match (findCurrentIndex config) with
        | Success(index) -> Playing(playList, index, (getElapsedTime config))
        | Failure(msg) -> Broken msg
    | _ -> drive

let private stop config schedule drive = 
    match drive with
    | Playing(playList, _, _) -> 
        let realStop d = 
            runSapControlProcess config "Stop"
            Loaded playList
        schedule realStop
        Stopping playList
    | Paused(playList, _) -> 
        writePlayListFile config playList
        Loaded playList
    | _ -> drive

//let private pause config callback drive = 
//    match drive with
//    | Playing(playList, idx) -> 
//        callback (Pausing(playList, idx))
//        runSapControlProcess config "Pause"
//        callback (Paused(playList, idx))
//    | _ -> callback drive
//
//let private next config callback drive = 
//    match drive with
//    | Playing(pl, tr) when not (isLastTrack pl tr) -> 
//        callback (GoingToNext(pl, tr))
//        runSapControlProcess config "Next"
//    | _ -> callback drive
//
//let private previous config callback drive = 
//    match drive with
//    | Playing(pl, tr) when not (isFirstTrack tr) -> 
//        callback (GoingToPrevious(pl, tr))
//        runSapControlProcess config "Prev"
//    | _ -> callback drive
let private handlePlayListChanged config drive = 
    match (findCurrentIndex config) with
    | Failure(msg) -> Broken msg
    | Success(index) -> 
        match index with
        | TrackIndex(_) -> 
            let reportPlaying pl = Playing(pl, index, (getElapsedTime config))
            match drive with
            | Starting(playList) -> reportPlaying playList
            | Playing(playList, _, _) -> reportPlaying playList
            | GoingToNext(playList, _) -> reportPlaying playList
            | GoingToPrevious(playList, _) -> reportPlaying playList
            | _ -> drive

let private shouldTriggerUpdate lastDrive newDrive (lastUpdate : DateTime) (now : DateTime) = 
    let supressIdentialUpdatesInterval = TimeSpan.FromMilliseconds(200.0)
    if now.Subtract(lastUpdate) > supressIdentialUpdatesInterval then true
    else 
        match lastDrive, newDrive with
        | Playing(_, _, elapsed1), Playing(pl2, t2, _) -> 
            let toCompareTo = Playing(pl2, t2, elapsed1)
            lastDrive <> toCompareTo
        | d1, d2 -> d1 <> d2

let createApi (config : ConfigData) = 
    let directEvents = Event<Drive>()
    
    let commandReceiver = 
        MailboxProcessor.Start(fun inbox -> 
            let schedule a = inbox.Post(NextAction a)
            
            let rec messageLoop drive lastLoopExecution = 
                async { 
                    let! internalCmd = inbox.Receive()
                    let newDrive = 
                        match internalCmd with
                        | ExternalCommand cmd -> 
                            match cmd with
                            | SetPlayList(pl) -> setPlayList drive pl
                            | Play -> play config drive
                            | Stop -> stop config schedule drive
                            //                        | Pause -> pause config internalCallback drive
                            //                        | Next -> next config internalCallback drive
                            //                        | Previous -> previous config internalCallback drive
                            //                        | Query -> ()
                            | _ -> drive
                        | PlayListChange _ -> handlePlayListChanged config drive
                        | NextAction(a) -> a drive
                    
                    let now = DateTime.Now
                    if shouldTriggerUpdate drive newDrive lastLoopExecution now then directEvents.Trigger newDrive
                    return! messageLoop newDrive now
                }
            messageLoop NoDisk DateTime.Now)
    
    let playListWatcher = new FileSystemWatcher(config.SAPDirectory, Path.GetFileName config.SAPPlaylistPath)
    playListWatcher.EnableRaisingEvents <- true
    playListWatcher.Changed
    |> Observable.subscribe (fun _ -> commandReceiver.Post PlayListChange)
    |> ignore
    let impl cmd = commandReceiver.Post(ExternalCommand cmd)
    { handle = impl
      changes = directEvents.Publish }


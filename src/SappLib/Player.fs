module Player

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
    | NoTrack
    | TrackIndex of int

type Drive = 
    | NoDisk
    | Loaded of PlayList
    | Playing of PlayList * CurrentTrack
    | Paused of PlayList * CurrentTrack
    | Starting of PlayList
    | Stopping of PlayList
    | Pausing of PlayList * CurrentTrack
    | Resuming of PlayList * CurrentTrack
    | GoingToNext of PlayList * CurrentTrack
    | GoingToPrevious of PlayList * CurrentTrack
    | Broken of string

type Command = 
    | SetPlayList of PlayList
    | Play
    | Stop
    | Pause
    | Next
    | Previous

type Api = 
    { handle : Command -> unit }

type CallBack = Drive -> unit

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

let findCurrentIndex config = 
    match (openPlayListFile config 10) with
    | Success(playListContent) -> 
        let index = playListContent |> Array.tryFindIndex (fun l -> l.StartsWith "#")
        match index with
        | Some(i) -> Success(TrackIndex i)
        | None -> Success(NoTrack)
    | Failure(msg) -> Failure(sprintf "Could nor retrieve current track (Reason: %s)" msg)

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

let private setPlayList callback drive playList = 
    match drive with
    | NoDisk -> callback (Loaded playList)
    | _ -> failwith "PlayList is already set" // TODO: stop and set new PlayList

let private play config callback drive = 
    match drive with
    | Loaded(playList) -> 
        writePlayListFile config playList
        startSapProcess config
        callback (Starting playList)
    | Paused(playList, _) -> 
        startSapProcess config
        match (findCurrentIndex config) with
        | Success(index) -> callback (Playing(playList, index))
        | Failure(msg) -> callback (Broken msg)
    | _ -> callback drive

let private stop config callback drive = 
    match drive with
    | Playing(playList, _) -> 
        callback (Stopping playList)
        runSapControlProcess config "Stop"
        callback (Loaded playList)
    | Paused(playList, _) -> 
        writePlayListFile config playList
        callback (Loaded playList)
    | _ -> callback drive

let private handlePlayListChanged config drive callback = 
    match (findCurrentIndex config) with
    | Failure(msg) -> callback (Broken msg)
    | Success(index) ->
        match index with
        | TrackIndex (i) -> 
            let reportPlaying pl = callback (Playing(pl, index))
            match drive with
            | Starting(playList) -> reportPlaying playList
            | Playing(playList, _) -> reportPlaying playList
            | _ -> ()
        | _ -> ()

#nowarn "40"

let createApi (config : ConfigData) callback = 
    let mutable drive = NoDisk
    
    let statusSender = 
        MailboxProcessor.Start(fun inbox -> 
            let rec messageLoop = 
                async { 
                    let! d = inbox.Receive()
                    drive <- d
                    callback d
                    return! messageLoop
                }
            messageLoop)
    
    let internalCallback d = statusSender.Post d
    
    let commandReceiver = 
        MailboxProcessor.Start(fun inbox -> 
            let rec messageLoop = 
                async { 
                    let! cmd = inbox.Receive()
                    match cmd with
                    | SetPlayList(pl) -> setPlayList internalCallback drive pl
                    | Play -> play config internalCallback drive
                    | Stop -> stop config internalCallback drive
                    | _ -> failwith "Not implemented."
                    return! messageLoop
                }
            messageLoop)
    
    let playListWatcher = new FileSystemWatcher(config.SAPDirectory, Path.GetFileName config.SAPPlaylistPath)
    playListWatcher.EnableRaisingEvents <- true
    playListWatcher.Changed.Add(fun ea -> 
        printf "\nPlayList change type: %O\n" ea.ChangeType
        handlePlayListChanged config drive internalCallback)
    let impl cmd = commandReceiver.Post cmd
    { handle = impl }


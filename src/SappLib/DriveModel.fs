module DriveModel

open System

type MusicFile = 
    | MusicFile of string

type PlayList = 
    | PlayList of MusicFile list

type TrackIndex = 
    | TrackIndex of int

type DriveState = 
    | Empty
    | Ready of PlayList
    | Playing of PlayList * TrackIndex * elapsed : TimeSpan
    | Paused of PlayList * TrackIndex
    | Starting of PlayList
    | Stopping of PlayList
    | Pausing of PlayList * TrackIndex
    | Resuming of PlayList * TrackIndex
    | GoingToNext of PlayList * TrackIndex
    | GoingToPrevious of PlayList * TrackIndex
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
    { execute : Command -> unit
      changes : IObservable<DriveState> }

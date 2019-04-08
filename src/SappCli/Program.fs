module Main

open System

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let config = Config.fromSettings()
    match SelectFlacFiles.getFiles config.FLACRootPath with
    | Some(files) -> (printf "%A" files)
    | _ -> printf "None"
    0 // return an integer exit code

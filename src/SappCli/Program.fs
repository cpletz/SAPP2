// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Microsoft.WindowsAPICodePack.Dialogs
open System

let selectFlacFiles() =

    use dlg = new CommonOpenFileDialog("Open FLAC MusicFIles")
    dlg.DefaultExtension <- "FLAC"
    dlg.Multiselect <- true
    dlg.Filters.Add(CommonFileDialogFilter("FLAC Files", "flac"))


    let dlgResult = dlg.ShowDialog()
    if dlgResult = CommonFileDialogResult.Ok then
        dlg.FileName
    else
        ""

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let file = selectFlacFiles()
    printfn "%s" file
    0 // return an integer exit code

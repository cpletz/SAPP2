module SelectFlacFiles

open Microsoft.WindowsAPICodePack.Dialogs

let getFiles rootPath =

    use dlg = new CommonOpenFileDialog("Open FLAC Files")
    dlg.DefaultExtension <- "FLAC"
    dlg.Multiselect <- true
    dlg.Filters.Add(CommonFileDialogFilter("FLAC Files", "flac"))
    dlg.DefaultDirectory <- rootPath

    if dlg.ShowDialog() = CommonFileDialogResult.Ok then
        dlg.FileNames 
        |> Seq.toList
        |> Some
    else
        None
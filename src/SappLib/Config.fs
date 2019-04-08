module Config

open System.Configuration
open System.IO
open System

type ConfigData = 
    { SAPExePath : string
      SOXExePath : string
      WAVCachePath : string
      TargetBitRate : int
      TargetSamplingRate : int
      FLACRootPath : string }
    member x.SAPDirectory = Path.GetDirectoryName x.SAPExePath
    member x.SAPPlaylistPath = 
        let exe = x.SAPExePath
        exe.Substring(0, exe.Length - 4) + ".m3u"

let fromSettings() = 
    let sapPath = ConfigurationManager.AppSettings.["SAPP/SAPPath"]
    let soxPath = ConfigurationManager.AppSettings.["SAPP/SOXPath"]
    let wavCachePath = ConfigurationManager.AppSettings.["SAPP/WAVCachePath"]
    let targetBitRate = ConfigurationManager.AppSettings.["SAPP/TargetBitRate"]
    let targetSamplingRate = ConfigurationManager.AppSettings.["SAPP/TargetSamplingRate"]
    let flacRootPath = ConfigurationManager.AppSettings.["SAPP/FLACRootPath"]
    { SAPExePath = sapPath
      SOXExePath = soxPath
      WAVCachePath = wavCachePath
      TargetBitRate = targetBitRate |> Int32.Parse
      TargetSamplingRate = targetSamplingRate |> Int32.Parse
      FLACRootPath = flacRootPath }

let empty =
    { SAPExePath = ""
      SOXExePath = ""
      WAVCachePath = ""
      TargetBitRate = 24
      TargetSamplingRate = 96000
      FLACRootPath = "" }
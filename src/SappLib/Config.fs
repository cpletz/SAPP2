module Config

open System.Configuration
open System.IO

type ConfigData = 
    { SAPExePath : string
      SOXExePath : string
      WAVCachePath : string
      ConvertArguments : string
      ConvertHDArguments : string
      FLACRootPath : string }
    member x.SAPDirectory = Path.GetDirectoryName x.SAPExePath
    member x.SAPPlaylistPath = 
        let exe = x.SAPExePath
        exe.Substring(0, exe.Length - 4) + ".m3u"

let fromSettings() = 
    let sapPath = ConfigurationManager.AppSettings.["SAPP/SAPPath"]
    let soxPath = ConfigurationManager.AppSettings.["SAPP/SOXPath"]
    let wavCachePath = ConfigurationManager.AppSettings.["SAPP/WAVCachePath"]
    let convertArguments = ConfigurationManager.AppSettings.["SAPP/ConvertArguments"]
    let convertHDArguments = ConfigurationManager.AppSettings.["SAPP/ConvertHDArguments"]
    let flacRootPath = ConfigurationManager.AppSettings.["SAPP/FLACRootPath"]
    { SAPExePath = sapPath
      SOXExePath = soxPath
      WAVCachePath = wavCachePath
      ConvertArguments = convertArguments
      ConvertHDArguments = convertHDArguments
      FLACRootPath = flacRootPath }

let empty =
    { SAPExePath = ""
      SOXExePath = ""
      WAVCachePath = ""
      ConvertArguments = ""
      ConvertHDArguments = ""
      FLACRootPath = "" }
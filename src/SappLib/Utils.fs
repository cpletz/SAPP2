module Utils

open Microsoft.FSharp.Reflection

let removeFromList item list =
    list |> List.filter (fun i -> i <> item)

let getUnionCaseName (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  
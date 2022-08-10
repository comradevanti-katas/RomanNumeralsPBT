[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module RomanNumeralsPBT.Roman

let convert i =
    let characters =
        List.replicate i ['I'; 'V']
        |> List.concat
    new string (Array.ofSeq characters)

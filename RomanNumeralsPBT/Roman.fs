[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module RomanNumeralsPBT.Roman

let convert i =
    let lastDigit = (i % 10) % 5
    if lastDigit = 0 then "V"
    elif lastDigit = 1 then "I"
    elif lastDigit = 2 then "II"
    elif lastDigit = 3 then "III"
    else "I"

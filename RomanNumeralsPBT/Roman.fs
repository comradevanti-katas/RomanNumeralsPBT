[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module RomanNumeralsPBT.Roman

let private symbolsWithValues =
    [ ("I", 1)
      ("IV", 4)
      ("V", 5)
      ("IX", 9)
      ("X", 10)
      ("XL", 40)
      ("L", 50)
      ("XC", 90)
      ("C", 100)
      ("CD", 400)
      ("D", 500)
      ("CM", 900)
      ("M", 1000) ]

let rec convert i =
    if i > 0 then
        let symbol, value =
            symbolsWithValues |> List.findBack (snd >> (>=) i)

        let rest = convert (i - value)
        symbol + rest
    else
        ""

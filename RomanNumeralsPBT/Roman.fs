[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module RomanNumeralsPBT.Roman

let private symbolsWithValues =
    List.rev [ ("I", 1)
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

let convert i =

    let rec addSymbolTo (num, s) (symbol, value) =
        if num >= value then
            addSymbolTo (num - value, s + symbol) (symbol, value)
        else
            (num, s)

    symbolsWithValues |> List.fold addSymbolTo (i, "") |> snd

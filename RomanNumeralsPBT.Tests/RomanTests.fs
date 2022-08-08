module RomanNumeralsPBT.RomanTests

open FsCheck
open FsCheck.Xunit

type ValidInput =
    static member Default() = Gen.choose (1, 3999) |> Arb.fromGen

type RomanPropertyAttribute() =
    inherit PropertyAttribute(Arbitrary = [| typeof<ValidInput> |])

[<RomanProperty>]
let ``At least one character`` (i: int) =
    i |> Roman.convert |> String.length >= 1
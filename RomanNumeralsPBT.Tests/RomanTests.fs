module RomanNumeralsPBT.RomanTests

open FsCheck
open FsCheck.Xunit

type ValidInput =
    static member Default() = Gen.choose (1, 3999) |> Arb.fromGen

type RomanPropertyAttribute() =
    inherit PropertyAttribute(Arbitrary = [| typeof<ValidInput> |])


let private validCharacters =
    Set.ofList [ 'I'; 'V'; 'X'; 'L'; 'C'; 'D'; 'M' ]

let private characterIsValid c = validCharacters |> Set.contains c


[<RomanProperty>]
let ``At least one character`` (i: int) =
    i |> Roman.convert |> String.length >= 1

[<RomanProperty>]
let ``Only valid characters`` (i: int) =
    i |> Roman.convert |> Seq.forall characterIsValid

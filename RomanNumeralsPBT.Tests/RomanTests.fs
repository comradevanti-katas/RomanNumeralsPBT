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
let ``At least one character`` i = i |> Roman.convert |> String.length >= 1

[<RomanProperty>]
let ``Only valid characters`` i =
    i |> Roman.convert |> Seq.forall characterIsValid

[<RomanProperty>]
let ``Different decimals make different romans`` i1 i2 =
    i1 <> i2 ==> lazy ((Roman.convert i1) <> (Roman.convert i2))

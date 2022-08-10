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
    let roman = i |> Roman.convert

    roman |> Seq.forall characterIsValid
    |@ $"Failing roman: %s{roman}"

[<RomanProperty>]
let ``Different decimals make different romans`` i1 i2 =
    i1 <> i2
    ==> lazy
        (let r1 = Roman.convert i1
         let r2 = Roman.convert i2

         r1 <> r2
         |@ $"%d{i1} and %d{i2} made the same roman (%s{r1})")

[<RomanProperty>]
let ``No character is repeated more than 3 times`` i =
    let roman = Roman.convert i

    roman
    |> Seq.windowed 4
    |> Seq.map Array.distinct
    |> Seq.map Array.length
    |> Seq.forall (fun letterCount -> letterCount >= 2)
    |@ $"Failing roman: %s{roman}"
module RomanNumeralsPBT.RomanTests

open FsCheck
open FsCheck.Xunit
open global.Xunit

type ValidInput =
    static member Default() = Gen.choose (1, 3999) |> Arb.fromGen

type RomanPropertyAttribute() =
    inherit PropertyAttribute(Arbitrary = [| typeof<ValidInput> |])


let private countPatternsForI =
    [ [ 1; 2; 3; 1; 0 ]
      [ 2; 3; 1; 0; 1 ]
      [ 3; 1; 0; 1; 2 ]
      [ 1; 0; 1; 2; 3 ]
      [ 0; 1; 2; 3; 1 ] ]

let private validCharacters =
    Set.ofList [ 'I'; 'V'; 'X'; 'L'; 'C'; 'D'; 'M' ]

let private characterIsValid c = validCharacters |> Set.contains c

let private countChar (c: char) (s: string) =
    s |> Seq.filter ((=) c) |> Seq.length

[<RomanProperty>]
let ``At least one character`` i = i |> Roman.convert |> String.length >= 1

[<RomanProperty>]
let ``Only valid characters`` i =
    i |> Roman.convert |> Seq.forall characterIsValid

[<Fact>]
let ``1 is I`` () = Roman.convert 1 = "I"

[<RomanProperty>]
let ``I follow correct count-pattern`` i =

    let inputs = List.init 5 ((+) i)
    let converted = inputs |> List.map Roman.convert
    let counts = converted |> List.map (countChar 'I')

    countPatternsForI |> List.exists ((=) counts)
    |@ $"%A{counts} matches no count-pattern for I"

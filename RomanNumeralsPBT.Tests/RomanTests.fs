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

let private withoutLast s =
    let length = s |> Seq.length
    s |> Seq.take (length - 1)


let onlyComesBefore allowed (roman: string) searched =
    let allowed = Set.ofSeq allowed

    let searchedIndices =
        roman
        |> Seq.indexed
        |> withoutLast
        |> Seq.filter (snd >> (=) searched)
        |> Seq.map fst

    let followingChars =
        searchedIndices
        |> Seq.map (fun index -> roman |> Seq.item (index + 1))

    followingChars
    |> Seq.forall (fun nextChar -> allowed |> Set.contains nextChar)
    |@ $"Failing roman: %s{roman}"

let onlyComesAfter allowed (roman: string) searched =
    let allowed = Set.ofSeq allowed

    let searchedIndices =
        roman
        |> Seq.indexed
        |> Seq.skip 1
        |> Seq.filter (snd >> (=) searched)
        |> Seq.map fst

    let precedingChars =
        searchedIndices
        |> Seq.map (fun index -> roman |> Seq.item (index - 1))

    precedingChars
    |> Seq.forall (fun precedingChar -> allowed |> Set.contains precedingChar)
    |@ $"Failing roman: %s{roman}"

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

[<RomanProperty>]
let ``Each character exists 0-4 times`` i =
    let roman = Roman.convert i

    validCharacters
    |> Set.forall (fun c -> roman |> Seq.filter ((=) c) |> Seq.length <= 4)
    |@ $"Failing roman: %s{roman}"

[<RomanProperty>]
let ``I may only come before I, V or X`` i =
    let roman = Roman.convert i
    'I' |> onlyComesBefore [ 'I'; 'V'; 'X' ] roman

[<RomanProperty>]
let ``V may only come before I`` i =
    let roman = Roman.convert i
    'V' |> onlyComesBefore [ 'I' ] roman

[<RomanProperty>]
let ``X may only come before I, V, X, L and C`` i =
    let roman = Roman.convert i
    'X' |> onlyComesBefore [ 'I'; 'V'; 'X'; 'L'; 'C' ] roman

[<RomanProperty>]
let ``X may only come after I, X, L, C, D and M`` i =
    let roman = Roman.convert i
    'X' |> onlyComesAfter [ 'I'; 'X'; 'L'; 'C'; 'D'; 'M' ] roman

[<RomanProperty>]
let ``L may only come before I, V and X`` i =
    let roman = Roman.convert i
    'L' |> onlyComesBefore [ 'I'; 'V'; 'X' ] roman

[<RomanProperty>]
let ``L may only come after X, C, D and M`` i =
    let roman = Roman.convert i
    'L' |> onlyComesAfter [ 'X'; 'C'; 'D'; 'M' ] roman

[<RomanProperty>]
let ``C may only come after X, C, D and M`` i =
    let roman = Roman.convert i
    'C' |> onlyComesAfter [ 'X'; 'C'; 'D'; 'M' ] roman

[<RomanProperty>]
let ``D may only come before I, V, X, L and C`` i =
    let roman = Roman.convert i
    'D' |> onlyComesBefore [ 'I'; 'V'; 'X'; 'L'; 'C' ] roman

[<RomanProperty>]
let ``D may only come after C and M`` i =
    let roman = Roman.convert i
    'D' |> onlyComesAfter [ 'C'; 'M' ] roman

[<RomanProperty>]
let ``M may only come after C and M`` i =
    let roman = Roman.convert i
    'M' |> onlyComesAfter [ 'C'; 'M' ] roman

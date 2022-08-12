# RomanNumeralsPBT
"Decimal to roman numerals" kata done with
property-based testing. Tests and
implementation in F#.

```fsharp
let rec convert i =
    if i >= 1000 then "M" + (convert (i - 1000))
    elif i >= 900 then "CM" + (convert (i - 900))
    elif i >= 500 then "D" + (convert (i - 500))
    elif i >= 400 then "CD" + (convert (i - 400))
    elif i >= 100 then "C" + (convert (i - 100))
    elif i >= 90 then "XC" + (convert (i - 90))
    elif i >= 50 then "L" + (convert (i - 50))
    elif i >= 40 then "XL" + (convert (i - 40))
    elif i >= 10 then "X" + (convert (i - 10))
    elif i >= 9 then "IX" + (convert (i - 9))
    elif i >= 5 then "V" + (convert (i - 5))
    elif i >= 4 then "IV" + (convert (i - 4))
    elif i >= 1 then "I" + (convert (i - 1))
    else ""
```
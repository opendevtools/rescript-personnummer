# rescript-personnummer

A _personnummer_ is a social security number in Sweden. This is early development of parsing such numbers in ReScript.

## Parse

```rescript
type t = {
  year: string,
  month: string,
  day: string,
  gender: Gender.t,
  placeOfBirth: PlaceOfBirth.t,
}

type t = Valid(t) | Invalid

Personnummer.Parse.make("1212121212")

// Valid({
//  year: "12",
//  month: "12",
//  day: "12",
//  gender: Male,
//  placeOfBirth: Stockholm,
//})
```

open Jest
open Expect
open Personnummer

testAll(
  "validates personnummer",
  list{
    ("1212121212", Validate.Valid("1212121212")),
    ("1212121213", Validate.Invalid),
    ("121212121212", Validate.Valid("121212121212")),
    ("191212121212", Validate.Valid("191212121212")),
    ("201212121212", Validate.Valid("201212121212")),
  },
  ((ssn, expected)) => expect(Validate.make(ssn)) |> toEqual(expected),
)

testAll(
  "parse",
  list{
    (
      "12 12 12 12 12",
      Parse.Valid({
        year: "12",
        month: "12",
        day: "12",
        gender: Male,
        placeOfBirth: Stockholm,
      }),
    ),
    ("12 12 12 12 13", Parse.Invalid),
  },
  ((ssn, expected)) => expect(Parse.make(ssn)) |> toEqual(expected),
)

testAll(
  "place of birth",
  list{
    ("Stockholm", 00, PlaceOfBirth.Stockholm),
    ("Stockholm", 13, Stockholm),
    ("Uppsala", 14, PlaceOfBirth.Uppsala),
    (`Södermanland`, 17, PlaceOfBirth.Sodermanland),
    (`Östergötaland`, 22, PlaceOfBirth.Ostergotaland),
    (`Jönköping`, 25, PlaceOfBirth.Jonkoping),
    ("Kronoberg", 27, PlaceOfBirth.Kronoberg),
    ("Kalmar", 30, PlaceOfBirth.Kalmar),
    ("Gotland", 32, PlaceOfBirth.Gotland),
    ("Blekinge", 33, PlaceOfBirth.Blekinge),
    ("Kristianstad", 36, PlaceOfBirth.Kristianstad),
    ("Malmohus", 40, PlaceOfBirth.Malmohus),
    ("Halland", 46, PlaceOfBirth.Halland),
    ("Goteborg", 49, PlaceOfBirth.Goteborg),
    ("Alvsborg", 56, PlaceOfBirth.Alvsborg),
    ("Skaraborg", 60, PlaceOfBirth.Skaraborg),
    ("Varmland", 63, PlaceOfBirth.Varmland),
    ("Orebro", 67, PlaceOfBirth.Orebro),
    ("Vastmanland", 69, PlaceOfBirth.Vastmanland),
    ("Kopparberg", 72, PlaceOfBirth.Kopparberg),
    ("Gavleborg", 76, PlaceOfBirth.Gavleborg),
    ("Vasternorrland", 79, PlaceOfBirth.Vasternorrland),
    ("Jamtland", 83, PlaceOfBirth.Jamtland),
    ("Vasterbotten", 87, PlaceOfBirth.Vasterbotten),
    ("Norrbotten", 90, PlaceOfBirth.Norrbotten),
    ("Extra number", 65, PlaceOfBirth.ExtraNumber),
    ("Extra number", 74, PlaceOfBirth.ExtraNumber),
    ("Extra number", 97, PlaceOfBirth.ExtraNumber),
    ("Unknown", 100, PlaceOfBirth.Unknown),
  },
  ((_title, value, expected)) => expect(PlaceOfBirth.make(value)) |> toBe(expected),
)

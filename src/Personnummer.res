let toInt = v => v->Belt.Int.fromString->Belt.Option.getWithDefault(0)

module Gender = {
  type t = Male | Female

  let make = v =>
    switch mod(toInt(v), 2) === 0 {
    | true => Female
    | false => Male
    }
}

module PlaceOfBirth = {
  type t =
    | Stockholm
    | Uppsala
    | Sodermanland
    | Ostergotaland
    | Jonkoping
    | Kronoberg
    | Kalmar
    | Gotland
    | Blekinge
    | Kristianstad
    | Malmohus
    | Halland
    | Goteborg
    | Alvsborg
    | Skaraborg
    | Varmland
    | Orebro
    | Vastmanland
    | Kopparberg
    | Gavleborg
    | Vasternorrland
    | Jamtland
    | Vasterbotten
    | Norrbotten
    | ExtraNumber
    | Unknown

  let isInRange = (c: int, min: int, max: int) => c >= min && c <= max

  let make = v => {
    switch v {
    | v if v->isInRange(0, 13) => Stockholm
    | v if v->isInRange(14, 15) => Uppsala
    | v if v->isInRange(16, 18) => Sodermanland
    | v if v->isInRange(19, 23) => Ostergotaland
    | v if v->isInRange(24, 26) => Jonkoping
    | v if v->isInRange(27, 28) => Kronoberg
    | v if v->isInRange(29, 31) => Kalmar
    | v if v === 32 => Gotland
    | v if v->isInRange(33, 34) => Blekinge
    | v if v->isInRange(35, 38) => Kristianstad
    | v if v->isInRange(39, 45) => Malmohus
    | v if v->isInRange(46, 47) => Halland
    | v if v->isInRange(48, 54) => Goteborg
    | v if v->isInRange(55, 58) => Alvsborg
    | v if v->isInRange(59, 61) => Skaraborg
    | v if v->isInRange(62, 64) => Varmland
    | v if v === 65 => ExtraNumber
    | v if v->isInRange(66, 68) => Orebro
    | v if v->isInRange(69, 70) => Vastmanland
    | v if v->isInRange(71, 73) => Kopparberg
    | v if v === 74 => ExtraNumber
    | v if v->isInRange(75, 77) => Gavleborg
    | v if v->isInRange(78, 81) => Vasternorrland
    | v if v->isInRange(82, 84) => Jamtland
    | v if v->isInRange(85, 88) => Vasterbotten
    | v if v->isInRange(89, 92) => Norrbotten
    | v if v->isInRange(93, 99) => ExtraNumber
    | _ => Unknown
    }
  }
}

type t = {
  year: string,
  month: string,
  day: string,
  gender: Gender.t,
  placeOfBirth: PlaceOfBirth.t,
}

module Normalize = {
  let make = ssn => ssn->Js.String2.replaceByRe(%re("/\D+/gi"), "")
}

module Validate = {
  type t = Valid(string) | Invalid

  let make = ssn => {
    open Js.String2

    let normalizedSsn = switch ssn->length {
    | 10 => ssn->substring(~from=0, ~to_=9)
    | _ => ssn->substring(~from=2, ~to_=11)
    }

    let luhnNumber =
      normalizedSsn
      ->split("")
      ->Js.Array2.mapi((v, i) =>
        (toInt(v) * (mod(i, 2) === 0 ? 2 : 1))->Belt.Int.toString->split("")->Js.Array2.map(toInt)
      )
      ->Js.Array2.concatMany([], _)
      ->Js.Array2.reduce((a, b) => a + b, 0)

    let control = mod(10 - mod(luhnNumber, 10), 10)
    let lastDigit = ssn->sliceToEnd(~from=-1)->Belt.Int.fromString->Belt.Option.getWithDefault(0)

    switch lastDigit === control {
    | true => Valid(ssn)
    | false => Invalid
    }
  }
}

module Parse = {
  type t = Valid(t) | Invalid

  module Capture = {
    type t = {
      year: string,
      month: string,
      day: string,
      birthNumber: string,
      gender: string,
    }

    type groups = {groups: t}

    external make: Js.Re.result => groups = "%identity"
  }

  let make = input => {
    switch input->Normalize.make->Validate.make {
    | Valid(ssn) =>
      switch Js.String2.length(ssn) {
      | 10 =>
        let t =
          ssn |> Js.Re.exec_(
            %re(
              "/^(?<year>\d{2})(?<month>\d{2})(?<day>\d{2})(?<birthNumber>\d{2})(?<gender>\d{1})/"
            ),
          )

        switch t {
        | Some(s) => {
            let {year, month, day, birthNumber, gender} = Capture.make(s).groups

            Valid({
              year: year,
              month: month,
              day: day,
              gender: Gender.make(gender),
              placeOfBirth: birthNumber
              ->Belt.Int.fromString
              ->Belt.Option.getWithDefault(0)
              ->PlaceOfBirth.make,
            })
          }
        | None => Invalid
        }
      | _ => Invalid
      }

    | Invalid => Invalid
    }
  }
}

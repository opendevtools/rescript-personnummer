// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_splice_call = require("rescript/lib/js/caml_splice_call.js");

function toInt(v) {
  return Belt_Option.getWithDefault(Belt_Int.fromString(v), 0);
}

function make(v) {
  if (Belt_Option.getWithDefault(Belt_Int.fromString(v), 0) % 2 === 0) {
    return /* Female */1;
  } else {
    return /* Male */0;
  }
}

var Gender = {
  make: make
};

function isInRange(c, min, max) {
  if (c >= min) {
    return c <= max;
  } else {
    return false;
  }
}

function make$1(v) {
  if (isInRange(v, 0, 13)) {
    return /* Stockholm */0;
  } else if (isInRange(v, 14, 15)) {
    return /* Uppsala */1;
  } else if (isInRange(v, 16, 18)) {
    return /* Sodermanland */2;
  } else if (isInRange(v, 19, 23)) {
    return /* Ostergotaland */3;
  } else if (isInRange(v, 24, 26)) {
    return /* Jonkoping */4;
  } else if (isInRange(v, 27, 28)) {
    return /* Kronoberg */5;
  } else if (isInRange(v, 29, 31)) {
    return /* Kalmar */6;
  } else if (v === 32) {
    return /* Gotland */7;
  } else if (isInRange(v, 33, 34)) {
    return /* Blekinge */8;
  } else if (isInRange(v, 35, 38)) {
    return /* Kristianstad */9;
  } else if (isInRange(v, 39, 45)) {
    return /* Malmohus */10;
  } else if (isInRange(v, 46, 47)) {
    return /* Halland */11;
  } else if (isInRange(v, 48, 54)) {
    return /* Goteborg */12;
  } else if (isInRange(v, 55, 58)) {
    return /* Alvsborg */13;
  } else if (isInRange(v, 59, 61)) {
    return /* Skaraborg */14;
  } else if (isInRange(v, 62, 64)) {
    return /* Varmland */15;
  } else if (v === 65) {
    return /* ExtraNumber */24;
  } else if (isInRange(v, 66, 68)) {
    return /* Orebro */16;
  } else if (isInRange(v, 69, 70)) {
    return /* Vastmanland */17;
  } else if (isInRange(v, 71, 73)) {
    return /* Kopparberg */18;
  } else if (v === 74) {
    return /* ExtraNumber */24;
  } else if (isInRange(v, 75, 77)) {
    return /* Gavleborg */19;
  } else if (isInRange(v, 78, 81)) {
    return /* Vasternorrland */20;
  } else if (isInRange(v, 82, 84)) {
    return /* Jamtland */21;
  } else if (isInRange(v, 85, 88)) {
    return /* Vasterbotten */22;
  } else if (isInRange(v, 89, 92)) {
    return /* Norrbotten */23;
  } else if (isInRange(v, 93, 99)) {
    return /* ExtraNumber */24;
  } else {
    return /* Unknown */25;
  }
}

var PlaceOfBirth = {
  isInRange: isInRange,
  make: make$1
};

function make$2(ssn) {
  return ssn.replace(/\D+/gi, "");
}

var Normalize = {
  make: make$2
};

function make$3(ssn) {
  var match = ssn.length;
  var normalizedSsn = match !== 10 ? ssn.substring(2, 11) : ssn.substring(0, 9);
  var __x = normalizedSsn.split("").map(function (v, i) {
        return String(Math.imul(Belt_Option.getWithDefault(Belt_Int.fromString(v), 0), i % 2 === 0 ? 2 : 1)).split("").map(toInt);
      });
  var luhnNumber = Caml_splice_call.spliceObjApply([], "concat", [__x]).reduce((function (a, b) {
          return a + b | 0;
        }), 0);
  var control = (10 - luhnNumber % 10 | 0) % 10;
  var lastDigit = Belt_Option.getWithDefault(Belt_Int.fromString(ssn.slice(-1)), 0);
  if (lastDigit === control) {
    return /* Valid */{
            _0: ssn
          };
  } else {
    return /* Invalid */0;
  }
}

var Validate = {
  make: make$3
};

var Capture = {};

function make$4(input) {
  var ssn = make$3(make$2(input));
  if (!ssn) {
    return /* Invalid */0;
  }
  var ssn$1 = ssn._0;
  var match = ssn$1.length;
  if (match !== 10) {
    return /* Invalid */0;
  }
  var t = /^(?<year>\d{2})(?<month>\d{2})(?<day>\d{2})(?<birthNumber>\d{2})(?<gender>\d{1})/.exec(ssn$1);
  if (t === null) {
    return /* Invalid */0;
  }
  var match$1 = t.groups;
  return /* Valid */{
          _0: {
            year: match$1.year,
            month: match$1.month,
            day: match$1.day,
            gender: make(match$1.gender),
            placeOfBirth: make$1(Belt_Option.getWithDefault(Belt_Int.fromString(match$1.birthNumber), 0))
          }
        };
}

var Parse = {
  Capture: Capture,
  make: make$4
};

exports.toInt = toInt;
exports.Gender = Gender;
exports.PlaceOfBirth = PlaceOfBirth;
exports.Normalize = Normalize;
exports.Validate = Validate;
exports.Parse = Parse;
/* No side effect */
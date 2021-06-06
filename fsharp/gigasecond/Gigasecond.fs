module Gigasecond
open System

let add (beginDate: DateTime) =
    beginDate.AddSeconds((float)1_000_000_000)
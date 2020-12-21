module EventTime exposing (toPosixFromIso)

import ISO8601 exposing (Time)
import Time exposing (Posix)


toPosixFromIso : String -> Posix
toPosixFromIso iso =
    case ISO8601.fromString iso of
        Err error ->
            Time.millisToPosix 0

        Ok time ->
            ISO8601.toPosix time

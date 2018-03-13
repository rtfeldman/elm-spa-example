module Date.Format exposing (format, formatISO8601, localFormat)

{-| Format strings for dates.
@docs format, localFormat, formatISO8601
-}

import Date.Local exposing (Local, international)
import Maybe exposing (andThen, withDefault)
import Regex
import String exposing (padLeft, right, toUpper)
import Time exposing (Posix)


re : Regex.Regex
re =
    Regex.fromString "%(_|-|0)?(%|Y|y|m|B|b|d|e|a|A|H|k|I|l|L|p|P|M|S)"
        |> Maybe.withDefault Regex.never


type Padding
    = NoPadding
    | Space
    | Zero
    | ZeroThreeDigits


{-| Use a format string to format a date. See the
[README](https://github.com/mgold/elm-date-format/blob/master/README.md) for a
list of accepted formatters.
-}
format : String -> Date.Date -> String
format s d =
    localFormat international s d


{-| Use a localization record and a format string to format a date. See the
[README](https://github.com/mgold/elm-date-format/blob/master/README.md) for a
list of accepted formatters.
-}
localFormat : Local -> String -> Date.Date -> String
localFormat loc s d =
    Regex.replace Regex.All re (formatToken loc d) s


{-| Formats a UTC date acording to
[ISO-8601](https://en.wikipedia.org/wiki/ISO_8601). This is commonly used to
send dates to a server. For example: `2016-01-06T09:22:00Z`.
-}
formatISO8601 : Date.Date -> String
formatISO8601 =
    format "%Y-%m-%dT%H:%M:%SZ"


formatToken : Local -> Date.Date -> Regex.Match -> String
formatToken loc d m =
    let
        ( padding, symbol ) =
            case m.submatches of
                [ Just "-", Just x ] ->
                    ( Just NoPadding, x )

                [ Just "_", Just x ] ->
                    ( Just Space, x )

                [ Just "0", Just x ] ->
                    ( Just Zero, x )

                [ Nothing, Just x ] ->
                    ( Nothing, x )

                _ ->
                    ( Nothing, " " )
    in
    case symbol of
        "%" ->
            "%"

        "Y" ->
            d |> Date.year |> toString

        "y" ->
            d |> Date.year |> toString |> right 2

        "m" ->
            d |> Date.month |> monthToInt |> padWith (withDefault Zero padding)

        "B" ->
            d |> Date.month |> monthToWord loc.date.months

        "b" ->
            d |> Date.month |> monthToWord loc.date.monthsAbbrev

        "d" ->
            d |> Date.day |> padWith (withDefault Zero padding)

        "e" ->
            d |> Date.day |> padWith (withDefault Space padding)

        "a" ->
            d |> Date.dayOfWeek |> dayOfWeekToWord loc.date.wdaysAbbrev

        "A" ->
            d |> Date.dayOfWeek |> dayOfWeekToWord loc.date.wdays

        "H" ->
            d |> Date.hour |> padWith (withDefault Zero padding)

        "k" ->
            d |> Date.hour |> padWith (withDefault Space padding)

        "I" ->
            d |> Date.hour |> mod12 |> zero2twelve |> padWith (withDefault Zero padding)

        "l" ->
            d |> Date.hour |> mod12 |> zero2twelve |> padWith (withDefault Space padding)

        "p" ->
            if Date.hour d < 12 then
                toUpper loc.time.am
            else
                toUpper loc.time.pm

        "P" ->
            if Date.hour d < 12 then
                loc.time.am
            else
                loc.time.pm

        "M" ->
            d |> Date.minute |> padWith (withDefault Zero padding)

        "S" ->
            d |> Date.second |> padWith (withDefault Zero padding)

        "L" ->
            d |> Date.millisecond |> padWith (withDefault ZeroThreeDigits padding)

        _ ->
            ""


monthToInt m =
    case m of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


monthToWord loc m =
    case m of
        Date.Jan ->
            loc.jan

        Date.Feb ->
            loc.feb

        Date.Mar ->
            loc.mar

        Date.Apr ->
            loc.apr

        Date.May ->
            loc.may

        Date.Jun ->
            loc.jun

        Date.Jul ->
            loc.jul

        Date.Aug ->
            loc.aug

        Date.Sep ->
            loc.sep

        Date.Oct ->
            loc.oct

        Date.Nov ->
            loc.nov

        Date.Dec ->
            loc.dec


dayOfWeekToWord loc dow =
    case dow of
        Date.Mon ->
            loc.mon

        Date.Tue ->
            loc.tue

        Date.Wed ->
            loc.wed

        Date.Thu ->
            loc.thu

        Date.Fri ->
            loc.fri

        Date.Sat ->
            loc.sat

        Date.Sun ->
            loc.sun


mod12 h =
    h % 12


zero2twelve n =
    if n == 0 then
        12
    else
        n


padWith : Padding -> a -> String
padWith padding =
    let
        padder =
            case padding of
                NoPadding ->
                    identity

                Zero ->
                    padLeft 2 '0'

                ZeroThreeDigits ->
                    padLeft 3 '0'

                Space ->
                    padLeft 2 ' '
    in
    padder << String.fromInt

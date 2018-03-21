module ISO8601
    exposing
        ( Offset
        , Time
        , Weekday(..)
        , add
        , day
        , diff
        , fromPosix
        , fromString
        , hour
        , millisecond
        , minute
        , month
        , offset
        , second
        , sub
        , toPosix
        , toString
        , weekday
        , year
        )

{-| This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`


# Time record

@docs Time, Weekday, Offset


# Accessors

@docs year, month, day, hour, minute, second, millisecond, offset, weekday


# Parsing

@docs fromString, toString


# Time conversion

@docs toPosix, fromPosix


# Manipulation

Note: The Time record has an offset, but not a time size. Adding or
subtracting time across a daylight savings boundary will NOT adjust the
offset.

@docs diff, sub, add

-}

import Array
import ISO8601.Extras exposing (..)
import Regex exposing (split)
import Result exposing (Result)
import String
import Time exposing (Posix)


regex str =
    Regex.fromString str
        |> Maybe.withDefault Regex.never



-- Model


{-| Offset represents the hour and minute timezone offset from UTC.
-}
type alias Offset =
    ( Int, Int )



-- integeger values for periods


ims : Int
ims =
    1


isec : Int
isec =
    ims * 1000


imin : Int
imin =
    isec * 60


ihour : Int
ihour =
    imin * 60


iday : Int
iday =
    ihour * 24


{-| Record representing the time. Offset is tuple representing the hour and minute ± from UTC.
-}
type alias Time =
    Model


type alias Model =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    , offset : ( Int, Int )
    }


defaultTime : Time
defaultTime =
    { year = 0
    , month = 1
    , day = 1
    , hour = 0
    , minute = 0
    , second = 0
    , millisecond = 0
    , offset = ( 0, 0 )
    }


{-| Represents one of the seven days of the week
-}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


fmt : Int -> String
fmt n =
    if n < 10 then
        "0" ++ String.fromInt n
    else
        String.fromInt n


fmtYear : Int -> String
fmtYear n =
    let
        s =
            String.fromInt n
    in
    if n < 10 then
        "000" ++ s
    else if n < 100 then
        "00" ++ s
    else if n < 1000 then
        "0" ++ s
    else
        s


fmtMs : Int -> String
fmtMs n =
    if n == 0 then
        ""
    else if n < 10 then
        ".00" ++ String.fromInt n
    else if n < 100 then
        ".0" ++ String.fromInt n
    else
        "." ++ String.fromInt n


fmtOffset : Offset -> String
fmtOffset offsetVal =
    case offsetVal of
        ( 0, 0 ) ->
            "Z"

        ( h, m ) ->
            let
                symbol =
                    if h >= 0 then
                        "+"
                    else
                        "-"
            in
            symbol ++ fmt (abs h) ++ fmt m


{-| Converts a Time record to an ISO 8601 formated string.
-}
toString : Time -> String
toString time =
    String.join ""
        [ fmtYear time.year
        , "-"
        , fmt time.month
        , "-"
        , fmt time.day
        , "T"
        , fmt time.hour
        , ":"
        , fmt time.minute
        , ":"
        , fmt time.second
        , fmtMs time.millisecond
        , fmtOffset time.offset
        ]


{-| Given an ISO 8601 compatible string, returns a Time record.

    ISO8601.fromString "2016-01-01T01:30:00-04:00"
    -- { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
        : ISO8601.Time
    ISO8601.fromString "2016-11-07"
    --{ year = 2016, month = 11, day = 7, hour = 0, minute = 0, second = 0, millisecond = 0, offset = (0,0) }
        : ISO8601.Time
    ```

-}
fromString : String -> Result String Time
fromString s =
    -- validate the string
    -- validate the numbers
    let
        parts =
            List.map .submatches (iso8601Regex s)

        unwrap : Maybe String -> String -> Int
        unwrap x d =
            x |> Maybe.withDefault d |> toInt
    in
    case parts of
        [ [ yearVal, monthVal, dayVal, hourVal, minuteVal, secondVal, millisecondVal, offsetVal, invalidVal ] ] ->
            case invalidVal of
                Just _ ->
                    Err "unexpected text"

                Nothing ->
                    validateTime
                        { year = unwrap yearVal "0"
                        , month = unwrap monthVal "1"
                        , day = unwrap dayVal "1"
                        , hour = unwrap hourVal "0"
                        , minute = unwrap minuteVal "0"
                        , second = unwrap secondVal "0"
                        , -- since the ms will possibly start with 0, add the 1 and get the remainder
                          -- ms' = (toInt ("1" ++ ms)) % 1000
                          millisecond = parseMilliseconds millisecondVal
                        , offset = parseOffset offsetVal
                        }

        _ ->
            Err "unknown error"


iso8601Regex : String -> List Regex.Match
iso8601Regex =
    Regex.findAtMost 1
        (regex
            ("(\\d{4})?-?"
                ++ -- year
                   "(\\d{2})?-?"
                ++ -- month
                   "(\\d{2})?"
                ++ -- DAY
                   "T?"
                ++ -- Time indicator
                   "(\\d{2})?:?"
                ++ -- hour
                   "(\\d{2})?:?"
                ++ -- minute
                   "(\\d{2})?"
                ++ -- second
                   "([.,]\\d{1,})?"
                ++ -- fractional second
                   "(Z|[+-]\\d{2}:?\\d{2})?"
                ++ -- offset
                   "(.*)?"
             -- invalid text
            )
        )


parseMilliseconds : Maybe String -> Int
parseMilliseconds msString =
    case msString of
        Nothing ->
            0

        Just s ->
            let
                decimalStr =
                    -- TODO this should be replaceAtMost 1
                    Regex.replace (regex "[,.]") (\_ -> "0.") s

                decimal =
                    String.toInt decimalStr |> Maybe.withDefault 0
            in
            1000 * decimal


parseOffset : Maybe String -> Offset
parseOffset timeString =
    let
        re =
            regex "(Z|([+-]\\d{2}:?\\d{2}))?"

        -- offset
        -- offset can be Z or ±h:mm ±hhmm or ±hh
        match =
            timeString
                |> Maybe.withDefault ""
                |> Regex.findAtMost 1 (regex "([-+])(\\d\\d):?(\\d\\d)")

        parts =
            List.map .submatches match

        setHour modifier hourVal =
            case modifier of
                "+" ->
                    hourVal

                "-" ->
                    modifier ++ hourVal

                -- this should never happen
                _ ->
                    hourVal
    in
    case parts of
        [ [ Just modifier, Just hourVal, Just minuteVal ] ] ->
            ( toInt (setHour modifier hourVal), toInt minuteVal )

        [ [ Just modifier, Just hourVal ] ] ->
            ( toInt (setHour modifier hourVal), 0 )

        _ ->
            ( 0, 0 )


offsetToTime : Time -> Int
offsetToTime time =
    let
        ( m, s ) =
            time.offset
    in
    (ihour * m) + (imin * s)


{-| Converts the Time to milliseconds relative to the Unix epoch: `1970-01-01T00:00:00Z`
-}
toPosix : Time -> Posix
toPosix time =
    toMillis time
        |> Time.millisToPosix


toMillis : Time -> Int
toMillis time =
    case time.year >= 1970 of
        False ->
            let
                years =
                    List.map daysInYear (List.range (time.year + 1) (1970 - 1))

                totalDays =
                    List.map (daysInMonth time.year) (List.range 1 time.month)
                        |> List.sum

                tots =
                    [ iday * List.sum years
                    , iday * (daysInYear time.year - totalDays)
                    , iday * (daysInMonth time.year time.month - time.day)
                    , iday - ihour - (ihour * time.hour)
                    , ihour - imin - (imin * time.minute)
                    , imin - (isec * time.second)
                    , offsetToTime time
                    ]
            in
            0 - (List.sum tots - time.millisecond)

        True ->
            let
                years =
                    List.map daysInYear (List.range 1970 (time.year - 1))

                months =
                    List.map (daysInMonth time.year) (List.range 1 (time.month - 1))

                tots =
                    [ iday * List.sum years
                    , iday * List.sum months
                    , iday * (time.day - 1)
                    , ihour * time.hour
                    , imin * time.minute
                    , isec * time.second
                    , -1 * offsetToTime time
                    ]
            in
            List.sum tots + time.millisecond


{-| Converts the milliseconds relative to the Unix epoch to a Time record.
-}
fromPosix : Posix -> Time
fromPosix posix =
    Time.posixToMillis posix
        |> fromMillis


{-| Converts the milliseconds relative to the Unix epoch to a Time record.
-}
fromMillis : Int -> Time
fromMillis ms =
    let
        millisecondsVal =
            modBy isec ms

        v =
            if ms >= 0 then
                After
            else
                Before
    in
    case v of
        After ->
            let
                -- additional days, the first day is implied
                daysVal =
                    ms // iday

                secondsVal =
                    ms // modBy 60 isec

                minutesVal =
                    ms // modBy 60 imin

                hoursVal =
                    ms // modBy 24 ihour

                ( yearsVal, remainingDays ) =
                    daysToYears After 1970 daysVal

                ( monthVal, daysInMonth ) =
                    daysToMonths yearsVal 1 (remainingDays + 1)
            in
            { defaultTime
                | second = secondsVal
                , minute = minutesVal
                , hour = hoursVal
                , day = daysInMonth
                , month = monthVal
                , year = yearsVal
                , millisecond = millisecondsVal
            }

        Before ->
            let
                rem =
                    modBy iday ms

                totalDays =
                    ms // iday

                -- this is right at the start of a new day
                ( yearsVal, remainingDays ) =
                    if rem == 0 then
                        daysToYears Before 1969 (totalDays + 1)
                    else
                        daysToYears Before 1969 totalDays

                ( monthVal, daysInMonth ) =
                    daysToMonths yearsVal 1 remainingDays

                daysVal =
                    rem // iday

                secondsVal =
                    rem // modBy 60 isec

                minutesVal =
                    rem // modBy 60 imin

                hoursVal =
                    rem // modBy 24 ihour
            in
            { defaultTime
                | second = secondsVal
                , minute = minutesVal
                , hour = hoursVal
                , day = daysInMonth
                , month = monthVal
                , year = yearsVal
                , millisecond = millisecondsVal
            }


validateHour : Time -> Result String Time
validateHour time =
    let
        h =
            time.hour

        m =
            time.minute

        s =
            time.second
    in
    if h == 24 && (m + s) > 0 then
        Err "hour is out of range"
    else if h < 0 || h > 24 then
        Err "hour is out of range"
    else if m < 0 || m > 59 then
        Err "minute is out of range"
    else if s < 0 || s > 59 then
        Err "second is out of range"
    else
        Ok time


validateTime : Time -> Result String Time
validateTime time =
    let
        maxDays =
            daysInMonth
    in
    if time.month < 1 || time.month > 12 then
        Err "month is out of range"
    else if time.day < 1 || time.day > daysInMonth time.year time.month then
        Err "day is out of range"
    else
        validateHour time


{-| return the year
-}
year : Time -> Int
year time =
    time.year


{-| return the month
-}
month : Time -> Int
month time =
    time.month


{-| return the day
-}
day : Time -> Int
day time =
    time.day


{-| return the hour
-}
hour : Time -> Int
hour time =
    time.hour


{-| return the minute
-}
minute : Time -> Int
minute time =
    time.minute


{-| return the secon
-}
second : Time -> Int
second time =
    time.second


{-| return the millisecond
-}
millisecond : Time -> Int
millisecond time =
    time.millisecond


{-| return the offset
-}
offset : Time -> Offset
offset time =
    time.offset


{-| Returns the day of the week from the Time record
-}
weekday : Time -> Weekday
weekday time =
    let
        daysFromEpoch =
            Array.fromList [ Thu, Fri, Sat, Sun, Mon, Tue, Wed ]

        daysSinceEpoch =
            ({ defaultTime
                | year = time.year
                , month = time.month
                , day = time.day
             }
                |> toMillis
            )
                // iday

        dayVal =
            Array.get (modBy 7 daysSinceEpoch) daysFromEpoch
    in
    case dayVal of
        Just d ->
            d

        -- this should never be reached
        Nothing ->
            Sun



-- Manipulation


{-| the difference bewteen two Time records in milliseconds
-}
diff : Time -> Time -> Int
diff a b =
    toMillis a - toMillis b


{-| Subtract milliseconds from a Time records
-}
sub : Time -> Int -> Time
sub time amount =
    toMillis time - amount |> fromTimeWithOffset time.offset


{-| Add milliseconds to a Time records
-}
add : Time -> Int -> Time
add time amount =
    toMillis time + amount |> fromTimeWithOffset time.offset


offsetToMS : Offset -> Int
offsetToMS offsetVal =
    let
        ( hourVal, minutesVal ) =
            offsetVal
    in
    (hourVal * 60 * 60 * 1000) + (minutesVal * 60 * 1000)


fromTimeWithOffset : Offset -> Int -> Time
fromTimeWithOffset offsetVal unix =
    let
        new =
            fromMillis (unix + offsetToMS offsetVal)
    in
    { new | offset = offsetVal }

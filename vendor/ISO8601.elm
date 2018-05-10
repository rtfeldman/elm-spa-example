module ISO8601 exposing (toPosix, toString)

import Parser exposing ((|.), (|=), Parser, int, map, oneOf, succeed, symbol)
import Time exposing (Month(..), utc)


{-| Convert from an ISO-8601 date string to a Posix time.

ISO-8601 date strings sometimes specify things in UTC. Other times, they specify
a non-UTC time as well as a UTC offset. I view the choice to support UTC offsets
as a design flaw in ISO-8601 strings. This function corrects this flaw by
normalizing dates into UTC regardless of how they were specified.

For example, if an ISO-8601 date string specifies "9am in UTC+2", this function
will return 7am UTC. The UTC offset is automatically factored in, then discarded.

-}
toPosix : String -> Result (List Parser.DeadEnd) Time.Posix
toPosix str =
    Parser.run iso8601 str


fromParts : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Time.Posix
fromParts year month day hour minute second ms utcOffsetMinutes =
    Time.millisToPosix
        -- TODO calculate how many leap years happened in this period, and adjust accordingly
        ((year * 365 * 24 * 60 * 60 * 1000)
            + -- TODO calculate days-per-month accurately
              (month * 12 * 24 * 60 * 60 * 1000)
            + (day * 24 * 60 * 60 * 1000)
            + (hour * 60 * 60 * 1000)
            -- Incoroprate and discard UTC offset
            + ((minute - utcOffsetMinutes) * 60 * 1000)
            + (second * 1000)
            + ms
        )


{-| From <https://www.timeanddate.com/date/leapyear.html>

In the Gregorian calendar three criteria must be taken into account to identify leap years:

  - The year can be evenly divided by 4;
  - If the year can be evenly divided by 100, it is NOT a leap year, unless;
  - The year is also evenly divisible by 400. Then it is a leap year.

This means that in the Gregorian calendar, the years 2000 and 2400 are leap years, while 1800, 1900, 2100, 2200, 2300 and 2500 are NOT leap years.

-}
isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))


{-| YYYY-MM-DDTHH:mm:ss.sssZ or ±YYYYYY-MM-DDTHH:mm:ss.sssZ
-}
iso8601 : Parser Time.Posix
iso8601 =
    -- TODO account for format variations, including those with UTC offsets
    succeed fromParts
        |= int
        -- YYYY
        |. symbol "-"
        |= int
        -- MM
        |. symbol "-"
        |= int
        -- DD
        |. symbol "T"
        |= int
        -- HH
        |. symbol ":"
        |= int
        -- mm
        |. symbol ":"
        |= int
        -- ss
        |. symbol ":"
        |= int
        -- sss
        |= oneOf
            [ -- "Z" means UTC
              map (\_ -> 0) (symbol "Z")

            -- +05:00 means UTC+5 whereas -11:30 means UTC-11.5
            , succeed utcOffsetMinutesFromParts
                |= oneOf
                    [ map (\_ -> 1) (symbol "+")
                    , map (\_ -> -1) (symbol "-")
                    ]
                |= int
                |. symbol ":"
                |= int
            ]


utcOffsetMinutesFromParts : Int -> Int -> Int -> Int
utcOffsetMinutesFromParts multiplier hours minutes =
    -- multiplier is either 1 or -1 (for negative UTC offsets)
    multiplier * ((hours * 60) + minutes)


{-| Inflate a Posix integer into a more memory-intensive ISO-8601 date string.

It's generally best to avoid doing this unless an external API requires it.

(UTC integers are less error-prone, take up less memory, and are more efficient
for time arithmetic.)

Format: YYYY-MM-DDTHH:mm:ss.sssZ

-}
toString : Time.Posix -> String
toString time =
    ---- YYYY
    toPaddedString 4 (Time.toYear utc time)
        ++ "-"
        -- MM
        ++ fromMonth (Time.toMonth utc time)
        ++ "-"
        -- DD
        ++ toPaddedString 2 (Time.toDay utc time)
        ++ "T"
        -- HH
        ++ toPaddedString 2 (Time.toHour utc time)
        ++ ":"
        -- mm
        ++ toPaddedString 2 (Time.toMinute utc time)
        ++ ":"
        -- ss
        ++ toPaddedString 2 (Time.toSecond utc time)
        ++ ":"
        -- sss
        ++ toPaddedString 2 (Time.toMillis utc time)
        ++ "Z"


toPaddedString digits time =
    String.padLeft digits '0' (String.fromInt time)


fromMonth : Time.Month -> String
fromMonth month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"

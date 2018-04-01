module ISO8601 exposing (toPosix, toString)

import Parser exposing ((|.), (|=), Parser, int, succeed, symbol)
import Time


{-| Convert from an ISO-8601 date string to a Posix time.

ISO-8601 date strings sometimes specify things in UTC. Other times, they specify
a non-UTC time as well as a UTC offset. I view the choice to support UTC offsets
as a design flaw in ISO-8601 strings. This function corrects this flaw by
normalizing dates into UTC regardless of how they were specified.

For example, if an ISO-8601 date string specifies "9am in UTC+2", this function
will return 7am UTC. The UTC offset is automatically factored in, then discarded.

-}
toPosix : String -> Result Parser.Error Time.Posix
toPosix str =
    Parser.run iso8601 str


fromParts : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Time.Posix
fromParts year month day hour minute second ms =
    Time.millisToPosix
        -- TODO account for leap years
        ((year * 365 * 24 * 60 * 60 * 1000)
            + -- TODO calculate months accurately
              (month * 12 * 24 * 60 * 60 * 1000)
            + (day * 24 * 60 * 60 * 1000)
            + (hour * 60 * 60 * 1000)
            + (minute * 60 * 1000)
            + (second * 1000)
            + ms
        )


{-| YYYY-MM-DDTHH:mm:ss.sssZ or ±YYYYYY-MM-DDTHH:mm:ss.sssZ
-}
iso8601 : Parser Time.Posix
iso8601 =
    -- TODO account for format variations
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
        |. symbol "Z"


{-| Inflate a Posix integer into a more memory-intensive ISO-8601 date string.

It's generally best to avoid doing this unless an external API requires it.

(UTC integers are less error-prone, take up less memory, and are more efficient
for time arithmetic.)

-}
toString : Time.Posix -> String
toString =
    Debug.crash "blah"

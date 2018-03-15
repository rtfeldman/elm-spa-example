module DateFormat
    exposing
        ( Token
        , amPmLowercase
        , amPmUppercase
        , dayOfMonthFixed
        , dayOfMonthNumber
        , dayOfMonthSuffix
        , dayOfWeekNameFirstThree
        , dayOfWeekNameFirstTwo
        , dayOfWeekNameFull
        , dayOfWeekNumber
        , dayOfWeekSuffix
        , dayOfYearFixed
        , dayOfYearNumber
        , dayOfYearSuffix
        , format
        , hourFixed
        , hourMilitaryFixed
        , hourMilitaryFromOneFixed
        , hourMilitaryFromOneNumber
        , hourMilitaryNumber
        , hourNumber
        , minuteFixed
        , minuteNumber
        , monthFixed
        , monthNameFirstThree
        , monthNameFull
        , monthNumber
        , monthSuffix
        , quarterNumber
        , quarterSuffix
        , secondFixed
        , secondNumber
        , text
        , weekOfYearFixed
        , weekOfYearNumber
        , weekOfYearSuffix
        , yearNumber
        , yearNumberLastTwo
        )

{-| A reliable way to format dates with elm


# The `format` function

@docs format


# Available formatting options

@docs Token


## Month

@docs monthNumber, monthSuffix, monthFixed, monthNameFirstThree, monthNameFull


## Day of the Month

@docs dayOfMonthNumber, dayOfMonthSuffix, dayOfMonthFixed


## Day of the Year

@docs dayOfYearNumber, dayOfYearSuffix, dayOfYearFixed


## Day of the Week

@docs dayOfWeekNumber, dayOfWeekSuffix, dayOfWeekNameFirstTwo, dayOfWeekNameFirstThree, dayOfWeekNameFull


## Year

@docs yearNumberLastTwo, yearNumber


## Quarter of the Year

@docs quarterNumber, quarterSuffix


## Week of the Year

@docs weekOfYearNumber, weekOfYearSuffix, weekOfYearFixed


## AM / PM

@docs amPmUppercase, amPmLowercase


## Hour

@docs hourMilitaryNumber, hourMilitaryFixed, hourNumber, hourFixed, hourMilitaryFromOneNumber, hourMilitaryFromOneFixed


## Minute

@docs minuteNumber, minuteFixed


## Second

@docs secondNumber, secondFixed


## Other Stuff

@docs text

-}

import Time exposing (Month(..), Posix, Weekday(..))


{-| Get the numeric value of the month.

Examples: `1, 2, 3, ... 11, 12`

-}
monthNumber : Token
monthNumber =
    MonthNumber


{-| Get the numeric value of the month, with a suffix at the end.

Examples: `1st, 2nd, 3rd, ... 11th, 12th`

-}
monthSuffix : Token
monthSuffix =
    MonthSuffix


{-| Get the numeric value of the month, fixed to two places.

Examples: `01, 02, 03, ... 11, 12`

-}
monthFixed : Token
monthFixed =
    MonthFixed


{-| Get the name of the month, but just the first three letters.

Examples: `Jan, Feb, Mar, ... Nov, Dec`

-}
monthNameFirstThree : Token
monthNameFirstThree =
    MonthNameFirst 3


{-| Get the full name of the month.

Examples: `January, February, ... December`

-}
monthNameFull : Token
monthNameFull =
    MonthNameFull


{-| Get the numeric value of the day of the month.

Examples: `1, 2, 3, ... 30, 31`

-}
dayOfMonthNumber : Token
dayOfMonthNumber =
    DayOfMonthNumber


{-| Get the numeric value of the day of the month, with a suffix at the end.

Examples: `1st, 2nd, 3rd, ... 30th, 31st`

-}
dayOfMonthSuffix : Token
dayOfMonthSuffix =
    DayOfMonthSuffix


{-| Get the numeric value of the day of the month, fixed to two places.

Examples: `01, 02, 03, ... 30, 31`

-}
dayOfMonthFixed : Token
dayOfMonthFixed =
    DayOfMonthFixed


{-| Get the numeric value of the day of the year.

Examples: `1, 2, 3, ... 364, 365`

-}
dayOfYearNumber : Token
dayOfYearNumber =
    DayOfYearNumber


{-| Get the numeric value of the day of the year, with a suffix at the end.

Examples: `1st, 2nd, 3rd, ... 364th, 365th`

-}
dayOfYearSuffix : Token
dayOfYearSuffix =
    DayOfYearSuffix


{-| Get the numeric value of the day of the year, fixed to three places.

Examples: `001, 002, 003, ... 364, 365`

-}
dayOfYearFixed : Token
dayOfYearFixed =
    DayOfYearFixed


{-| Get the numeric value of the day of the week.

Examples: `0, 1, 2, ... 5, 6`

-}
dayOfWeekNumber : Token
dayOfWeekNumber =
    DayOfWeekNumber


{-| Get the numeric value of the day of the week, with a suffix at the end.

Examples: `0th, 1st, 2nd, ... 5th, 6th`

-}
dayOfWeekSuffix : Token
dayOfWeekSuffix =
    DayOfWeekSuffix


{-| Gets the name of the day of the week, but just the first two letters.

Examples: `Su, Mo, Tue, ... Fr, Sa`

-}
dayOfWeekNameFirstTwo : Token
dayOfWeekNameFirstTwo =
    DayOfWeekNameFirst 2


{-| Gets the name of the day of the week, but just the first three letters.

Examples: `Sun, Mon, Tue, ... Fri, Sat`

-}
dayOfWeekNameFirstThree : Token
dayOfWeekNameFirstThree =
    DayOfWeekNameFirst 3


{-| Gets the full name of the day of the week.

Examples: `Sunday, Monday, ... Friday, Saturday`

-}
dayOfWeekNameFull : Token
dayOfWeekNameFull =
    DayOfWeekNameFull


{-| Get the year, but just the last two letters.

Examples: `70, 71, ... 29, 30`

-}
yearNumberLastTwo : Token
yearNumberLastTwo =
    YearNumberLastTwo


{-| Get the year.

Examples: `1970, 1971, ... 2018, ... 9999, ...`

-}
yearNumber : Token
yearNumber =
    YearNumber


{-| Get the numeric value for the quarter of the year, with a suffix.

Examples: `1, 2, 3, 4`

-}
quarterNumber : Token
quarterNumber =
    QuarterNumber


{-| Get the numeric value for the quarter of the year, with a suffix.

Examples: `1st, 2nd, 3rd, 4th`

-}
quarterSuffix : Token
quarterSuffix =
    QuarterSuffix


{-| Get the numeric value for the week of the year.

Examples: `1, 2, 3, ... 51, 52`

-}
weekOfYearNumber : Token
weekOfYearNumber =
    WeekOfYearNumber


{-| Get the numeric value for the week of the year, with a suffix at the end.

Examples: `1st, 2nd, 3rd, ... 51st, 52nd`

-}
weekOfYearSuffix : Token
weekOfYearSuffix =
    WeekOfYearSuffix


{-| Get the numeric value for the week of the year, fixed to two places.

Examples: `01, 02, 03, ... 51, 52`

-}
weekOfYearFixed : Token
weekOfYearFixed =
    WeekOfYearFixed


{-| Get the AM / PM value of the hour, in uppercase.

Examples: `AM, PM`

-}
amPmUppercase : Token
amPmUppercase =
    AmPmUppercase


{-| Get the AM / PM value of the hour, in uppercase.

Examples: `am, pm`

-}
amPmLowercase : Token
amPmLowercase =
    AmPmLowercase


{-| Get the hour of the 24-hour day.

Examples: `0, 1, 2, ... 22, 23`

-}
hourMilitaryNumber : Token
hourMilitaryNumber =
    HourMilitaryNumber


{-| Get the hour of the 24-hour day, fixed to two places.

Examples: `00, 01, 02, ... 22, 23`

-}
hourMilitaryFixed : Token
hourMilitaryFixed =
    HourMilitaryFixed


{-| Get the hour of the 12-hour day.

Examples: `0, 1, 2, ... 11, 12`

-}
hourNumber : Token
hourNumber =
    HourNumber


{-| Get the hour of the 12-hour day, fixed to two places.

Examples: `00, 01, 02, ... 11, 12`

-}
hourFixed : Token
hourFixed =
    HourFixed


{-| Get the hour of the 24-hour day, starting from one.

Examples: `1, 2, ... 23, 24`

-}
hourMilitaryFromOneNumber : Token
hourMilitaryFromOneNumber =
    HourMilitaryFromOneNumber


{-| Get the hour of the 24-hour day, starting from one, fixed to two places.

Examples: `01, 02, ... 23, 24`

-}
hourMilitaryFromOneFixed : Token
hourMilitaryFromOneFixed =
    HourMilitaryFromOneFixed


{-| Get the minute of the hour.

Examples: `0, 1, 2, ... 58, 59`

-}
minuteNumber : Token
minuteNumber =
    MinuteNumber


{-| Get the minute of the hour, fixed to two places.

Examples: `00, 01, 02, ... 58, 59`

-}
minuteFixed : Token
minuteFixed =
    MinuteFixed


{-| Get the second of the minute.

Examples: `0, 1, 2, ... 58, 59`

-}
secondNumber : Token
secondNumber =
    SecondNumber


{-| Get the second of the minute, fixed to two places.

Examples: `00, 01, 02, ... 58, 59`

-}
secondFixed : Token
secondFixed =
    SecondFixed


{-| Represent a string value

    formatter : Date -> String
    formatter =
        DateFormat.format
            [ DateFormat.hourMilitaryFixed
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            ]

Could be given a date to return something like `"23:15"` or `"04:43"`

-}
text : String -> Token
text =
    Text


{-| These are the available tokens to help you format dates.
-}
type Token
    = MonthNumber
    | MonthSuffix
    | MonthFixed
    | MonthNameFirst Int
    | MonthNameFull
    | DayOfMonthNumber
    | DayOfMonthSuffix
    | DayOfMonthFixed
    | DayOfYearNumber
    | DayOfYearSuffix
    | DayOfYearFixed
    | DayOfWeekNumber
    | DayOfWeekSuffix
    | DayOfWeekNameFirst Int
    | DayOfWeekNameFull
    | YearNumberLastTwo
    | YearNumber
    | QuarterNumber
    | QuarterSuffix
    | WeekOfYearNumber
    | WeekOfYearSuffix
    | WeekOfYearFixed
    | AmPmUppercase
    | AmPmLowercase
    | HourMilitaryNumber
    | HourMilitaryFixed
    | HourNumber
    | HourFixed
    | HourMilitaryFromOneNumber
    | HourMilitaryFromOneFixed
    | MinuteNumber
    | MinuteFixed
    | SecondNumber
    | SecondFixed
    | Text String


{-| This function takes in a list of tokens and a date to create your formatted string!

Let's say `someDate` is on November 15, 1993 at 15:06.

    -- "15:06"
    format
        [ hourMilitaryFixed
        , text ":"
        , minuteFixed
        ]
        someDate

    -- "3:06 pm"
    format
        [ hourNumber
        , text ":"
        , minuteFixed
        , text " "
        , amPmLowercase
        ]
        someDate

    -- "Nov 15th, 1993"
    format
        [ monthNameFirstThree
        , text " "
        , dayOfMonthSuffix
        , text ", "
        , yearNumber
        ]
        someDate

-}
format : List Token -> Date -> String
format tokens date =
    tokens
        |> List.map (piece date)
        |> String.join ""


{-| Months of the year, in the correct order.
-}
months : List Month
months =
    [ Jan
    , Feb
    , Mar
    , Apr
    , May
    , Jun
    , Jul
    , Aug
    , Sep
    , Nov
    , Dec
    ]


{-| Days of the week, in the correct order.
-}
days : List Day
days =
    [ Sun
    , Mon
    , Tue
    , Wed
    , Thu
    , Fri
    , Sat
    ]


piece : Date -> Token -> String
piece date token =
    case token of
        MonthNumber ->
            monthNumber_ date
                |> toString

        MonthSuffix ->
            monthNumber_ date
                |> toSuffix

        MonthFixed ->
            monthNumber_ date
                |> toFixedLength 2

        MonthNameFirst num ->
            fullMonthName date
                |> String.left num

        MonthNameFull ->
            fullMonthName date

        QuarterNumber ->
            quarter date
                |> (+) 1
                |> toString

        QuarterSuffix ->
            quarter date
                |> (+) 1
                |> toSuffix

        DayOfMonthNumber ->
            dayOfMonth date
                |> toString

        DayOfMonthSuffix ->
            dayOfMonth date
                |> toSuffix

        DayOfMonthFixed ->
            dayOfMonth date
                |> toFixedLength 2

        DayOfYearNumber ->
            dayOfYear date
                |> toString

        DayOfYearSuffix ->
            dayOfYear date
                |> toSuffix

        DayOfYearFixed ->
            dayOfYear date
                |> toFixedLength 3

        DayOfWeekNumber ->
            dayOfWeek date
                |> toString

        DayOfWeekSuffix ->
            dayOfWeek date
                |> toSuffix

        DayOfWeekNameFirst num ->
            dayOfWeekName date
                |> String.left num

        DayOfWeekNameFull ->
            dayOfWeekName date

        WeekOfYearNumber ->
            weekOfYear date
                |> toString

        WeekOfYearSuffix ->
            weekOfYear date
                |> toSuffix

        WeekOfYearFixed ->
            weekOfYear date
                |> toFixedLength 2

        YearNumberLastTwo ->
            year date
                |> String.right 2

        YearNumber ->
            year date

        AmPmUppercase ->
            amPm date
                |> String.toUpper

        AmPmLowercase ->
            amPm date
                |> String.toLower

        HourMilitaryNumber ->
            Date.hour date
                |> toString

        HourMilitaryFixed ->
            Date.hour date
                |> toFixedLength 2

        HourNumber ->
            Date.hour date
                |> toNonMilitary
                |> toString

        HourFixed ->
            Date.hour date
                |> toNonMilitary
                |> toFixedLength 2

        HourMilitaryFromOneNumber ->
            Date.hour date
                |> (+) 1
                |> toString

        HourMilitaryFromOneFixed ->
            Date.hour date
                |> (+) 1
                |> toFixedLength 2

        MinuteNumber ->
            Date.minute date
                |> toString

        MinuteFixed ->
            Date.minute date
                |> toFixedLength 2

        SecondNumber ->
            Date.second date
                |> toString

        SecondFixed ->
            Date.second date
                |> toFixedLength 2

        Text string ->
            string



-- | FractionalSecond Int
-- | UnixTimestamp
-- | UnixMillisecondTimestamp
-- | PlainText String
-- MONTHS


monthPair : Date -> ( Int, Month )
monthPair date =
    months
        |> List.indexedMap Tuple.pair
        |> List.filter (\( i, m ) -> m == Date.month date)
        |> List.head
        |> Maybe.withDefault ( 0, Jan )


monthNumber_ : Date -> Int
monthNumber_ date =
    monthPair date
        |> (\( i, m ) -> i)
        |> (+) 1


fullMonthName : Date -> String
fullMonthName date =
    case Date.month date of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


daysInMonth : Int -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29
            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    if year % 4 /= 0 then
        False
    else if year % 100 /= 0 then
        True
    else if year % 400 /= 0 then
        False
    else
        True



-- QUARTERS


quarter : Date -> Int
quarter date =
    monthNumber_ date // 4



-- DAY OF MONTH


dayOfMonth : Date -> Int
dayOfMonth =
    Date.day



-- DAY OF YEAR


dayOfYear : Date -> Int
dayOfYear date =
    let
        monthsBeforeThisOne : List Month
        monthsBeforeThisOne =
            List.take (monthNumber_ date - 1) months

        daysBeforeThisMonth : Int
        daysBeforeThisMonth =
            monthsBeforeThisOne
                |> List.map (daysInMonth (Date.year date))
                |> List.sum
    in
    daysBeforeThisMonth + dayOfMonth date



-- DAY OF WEEK


dayOfWeek : Date -> Int
dayOfWeek date =
    days
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, d ) -> d == Date.dayOfWeek date)
        |> List.head
        |> Maybe.withDefault ( 0, Sun )
        |> (\( i, d ) -> i)


dayOfWeekName : Date -> String
dayOfWeekName date =
    case Date.dayOfWeek date of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"



-- WEEK OF YEAR


type alias SimpleDate =
    { month : Month
    , day : Int
    , year : Int
    }


weekOfYear : Date -> Int
weekOfYear date =
    let
        daysSoFar : Int
        daysSoFar =
            dayOfYear date

        firstDay : Date
        firstDay =
            firstDayOfYear date

        firstDayOffset : Int
        firstDayOffset =
            dayOfWeek firstDay
    in
    (daysSoFar + firstDayOffset) // 7 + 1


firstDayOfYear : Date -> Date
firstDayOfYear date =
    case Date.fromString <| toString (Date.year date) ++ "-01-01T00:00:00.000Z" of
        Ok date ->
            date

        Err _ ->
            date



-- YEAR


year : Date -> String
year date =
    date
        |> Date.year
        |> toString



-- AM / PM


amPm : Date -> String
amPm date =
    if Date.hour date > 11 then
        "pm"
    else
        "am"



-- HOUR


toNonMilitary : Int -> Int
toNonMilitary num =
    if num == 0 then
        12
    else if num <= 12 then
        num
    else
        num - 12



-- GENERIC


toFixedLength : Int -> Int -> String
toFixedLength totalChars num =
    let
        numStr =
            toString num

        numZerosNeeded =
            totalChars - String.length numStr

        zeros =
            List.range 1 numZerosNeeded
                |> List.map (\_ -> "0")
                |> String.join ""
    in
    zeros ++ numStr


toSuffix : Int -> String
toSuffix num =
    let
        suffix =
            case num of
                11 ->
                    "th"

                12 ->
                    "th"

                13 ->
                    "th"

                _ ->
                    case num % 10 of
                        1 ->
                            "st"

                        2 ->
                            "nd"

                        3 ->
                            "rd"

                        _ ->
                            "th"
    in
    toString num ++ suffix

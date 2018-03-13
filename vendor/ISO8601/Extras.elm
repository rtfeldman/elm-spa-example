module ISO8601.Extras
    exposing
        ( EpochRelative(..)
        , daysInMonth
        , daysInYear
        , daysToMonths
        , daysToYears
        , isLeapYear
        , toInt
        )

import Array
import String


toInt : String -> Int
toInt str =
    String.toInt str
        |> Maybe.withDefault 0


isLeapYear : Int -> Bool
isLeapYear year =
    let
        -- A If the year is evenly divisible by 4, go to step B
        a =
            0 == year % 4

        -- B If the year is evenly divisible by 100, go to step C
        b =
            0 == year % 100

        -- C If the year is evenly divisible by 400, go to step D
        c =
            0 == year % 400
    in
    case [ a, b, c ] of
        [ True, True, True ] ->
            True

        [ True, False, _ ] ->
            True

        [ True, True, False ] ->
            False

        _ ->
            False


type alias CalMonth =
    ( String, Int, Int )


calendar : Array.Array CalMonth
calendar =
    Array.fromList
        [ ( "January", 31, 31 )
        , ( "February", 28, 29 )
        , ( "March", 31, 31 )
        , ( "April", 30, 30 )
        , ( "May", 31, 31 )
        , ( "June", 30, 30 )
        , ( "July", 31, 31 )
        , ( "August", 31, 31 )
        , ( "September", 30, 30 )
        , ( "October", 31, 31 )
        , ( "November", 30, 30 )
        , ( "December", 31, 31 )
        ]


daysInMonth : Int -> Int -> Int
daysInMonth year monthInt =
    let
        calMonth =
            Array.get (monthInt - 1) calendar
    in
    case calMonth of
        Just ( _, days, leapDays ) ->
            if isLeapYear year then
                leapDays
            else
                days

        Nothing ->
            0


daysInYear : Int -> Int
daysInYear year =
    if isLeapYear year then
        366
    else
        365


type EpochRelative
    = Before
    | After



-- from a starting year returns the ending year and remaing days


daysToYears : EpochRelative -> Int -> Int -> ( Int, Int )
daysToYears rel startYear remainingDays =
    case rel of
        After ->
            let
                remainingDays_ =
                    remainingDays - daysInYear startYear
            in
            if remainingDays_ > 0 then
                daysToYears After (startYear + 1) remainingDays_
            else if remainingDays_ == 0 then
                ( startYear + 1, 0 )
            else
                ( startYear, remainingDays )

        Before ->
            let
                remainingDays_ =
                    remainingDays + daysInYear startYear
            in
            if remainingDays_ < 0 then
                daysToYears Before (startYear - 1) remainingDays_
            else
                ( startYear, daysInYear startYear + remainingDays )



-- remaingDays will alawys be less than 366


daysToMonths : Int -> Int -> Int -> ( Int, Int )
daysToMonths year startMonth remainingDays =
    let
        remainingDays_ =
            remainingDays - daysInMonth year startMonth
    in
    if remainingDays_ > 0 then
        daysToMonths year (startMonth + 1) remainingDays_
    else
        ( startMonth, remainingDays )


yearsToDays : Int -> Int -> Int -> Int
yearsToDays ending current days =
    if ending > current then
        yearsToDays
            ending
            (current + 1)
            (daysInYear current)
    else
        days

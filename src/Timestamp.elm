module Timestamp exposing (format, iso8601Decoder, view)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Iso8601
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Time exposing (Month(..))



-- VIEW


view : Time.Zone -> Time.Posix -> Html msg
view timeZone timestamp =
    span [ class "date" ] [ text (format timeZone timestamp) ]



-- DECODE


{-| Decode an ISO-8601 date string.
-}
iso8601Decoder : Decoder Time.Posix
iso8601Decoder =
    Decode.string
        |> Decode.andThen fromString


fromString : String -> Decoder Time.Posix
fromString str =
    case Iso8601.toTime str of
        Ok successValue ->
            succeed successValue

        Err _ ->
            fail ("Invalid date: " ++ str)



-- FORMAT


{-| Format a timestamp as a String, like so:

    "February 14, 2018"

For more complex date formatting scenarios, here's a nice package:
<https://package.elm-lang.org/packages/ryannhg/date-format/latest/>

-}
format : Time.Zone -> Time.Posix -> String
format zone time =
    let
        month =
            case Time.toMonth zone time of
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

        day =
            String.fromInt (Time.toDay zone time)

        year =
            String.fromInt (Time.toYear zone time)
    in
    month ++ " " ++ day ++ ", " ++ year

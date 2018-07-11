module Util exposing (appendErrors, dateStringDecoder, formatTimestamp, onClickStopPropagation, pair, viewIf)

import Html exposing (Attribute, Html)
import Html.Events exposing (stopPropagationOn)
import Iso8601
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Parser
import Time exposing (Month(..))


{-| Useful when building up a Cmd via a pipeline, and then pairing it with
a model at the end.

    session.user
        |> User.Request.foo
        |> Task.attempt Foo
        |> pair { model | something = blah }

-}
pair : a -> b -> ( a, b )
pair first second =
    ( first, second )


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content

    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }


{-| Decode an ISO-8601 date string.
-}
dateStringDecoder : Decoder Time.Posix
dateStringDecoder =
    Decode.string
        |> Decode.andThen (\str -> fromResult str (Iso8601.toTime str))


fromResult : String -> Result (List Parser.DeadEnd) a -> Decoder a
fromResult source result =
    case result of
        Ok successValue ->
            succeed successValue

        Err _ ->
            fail ("Failed to parse: " ++ source)


formatTimestamp : Time.Zone -> Time.Posix -> String
formatTimestamp zone time =
    let
        -- This is where we'd do internationalization, if necessary!
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

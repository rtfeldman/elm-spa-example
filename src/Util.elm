module Util exposing (apiUrl, dateStringDecoder, formatTimestamp, onClickStopPropagation, updateFromResult)

import Html exposing (Attribute, Html)
import Html.Events exposing (stopPropagationOn)
import Iso8601
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Time exposing (Month(..))


apiUrl : String -> String
apiUrl str =
    "https://conduit.productionready.io/api" ++ str


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


updateFromResult : { model | errors : List error } -> cmd -> Result error cmd -> ( { model | errors : List error }, cmd )
updateFromResult model fallbackCmd result =
    case result of
        Ok cmd ->
            ( model, cmd )

        Err error ->
            ( { model | errors = model.errors ++ [ error ] }, fallbackCmd )


{-| Decode an ISO-8601 date string.
-}
dateStringDecoder : Decoder Time.Posix
dateStringDecoder =
    Decode.string
        |> Decode.andThen fromString


fromString : String -> Decoder Time.Posix
fromString str =
    case Iso8601.toTime str of
        Ok successValue ->
            succeed successValue

        Err _ ->
            fail ("Failed to parse: " ++ str)


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

module Util exposing (appendErrors, dateStringDecoder, onClickStopPropagation, pair, viewIf)

import Html exposing (Attribute, Html)
import Html.Events exposing (stopPropagationOn)
import ISO8601
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Parser
import Time exposing (Posix)


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
    { model | errors = List.append model.errors errors }


{-| Decode an ISO-8601 date string.
-}
dateStringDecoder : Decoder Posix
dateStringDecoder =
    Decode.string
        |> Decode.andThen (ISO8601.toPosix >> fromResult)


fromResult : Result Parser.Error a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            succeed successValue

        Err { source } ->
            fail ("Failed to parse: " ++ source)

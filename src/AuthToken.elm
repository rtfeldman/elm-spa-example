module AuthToken exposing (AuthToken, decoder, encode, withAuthorization)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type AuthToken
    = AuthToken String



-- CREATE


decoder : Decoder AuthToken
decoder =
    Decode.string
        |> Decode.map AuthToken



-- CONVERT


encode : AuthToken -> Value
encode (AuthToken str) =
    Encode.string str


withAuthorization : Maybe AuthToken -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeToken builder =
    case maybeToken of
        Just (AuthToken str) ->
            builder
                |> withHeader "authorization" ("Token " ++ str)

        Nothing ->
            builder

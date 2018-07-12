module UserPhoto exposing (UserPhoto, decoder, encode, src, toMaybeString)

import Html exposing (Attribute)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type UserPhoto
    = UserPhoto (Maybe String)



-- CREATE


decoder : Decoder UserPhoto
decoder =
    Decode.map UserPhoto (Decode.nullable Decode.string)



-- CONVERT


encode : UserPhoto -> Value
encode (UserPhoto maybeUrl) =
    maybeUrl
        |> Maybe.map Encode.string
        |> Maybe.withDefault Encode.null


src : UserPhoto -> Attribute msg
src photo =
    Html.Attributes.src (photoToUrl photo)


toMaybeString : UserPhoto -> Maybe String
toMaybeString (UserPhoto maybeUrl) =
    maybeUrl



-- INTERNAL


photoToUrl : UserPhoto -> String
photoToUrl (UserPhoto maybeUrl) =
    case maybeUrl of
        Nothing ->
            defaultPhotoUrl

        Just "" ->
            defaultPhotoUrl

        Just url ->
            url


defaultPhotoUrl : String
defaultPhotoUrl =
    "https://static.productionready.io/images/smiley-cyrus.jpg"

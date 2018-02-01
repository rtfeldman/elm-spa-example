module Data.UserPhoto exposing (UserPhoto, decoder, encode, src, toMaybeString)

import Html exposing (Attribute)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra


type UserPhoto
    = UserPhoto (Maybe String)


src : UserPhoto -> Attribute msg
src =
    photoToUrl >> Html.Attributes.src


decoder : Decoder UserPhoto
decoder =
    Decode.map UserPhoto (Decode.nullable Decode.string)


encode : UserPhoto -> Value
encode (UserPhoto maybeUrl) =
    EncodeExtra.maybe Encode.string maybeUrl


toMaybeString : UserPhoto -> Maybe String
toMaybeString (UserPhoto maybeUrl) =
    maybeUrl



-- INTERNAL --


photoToUrl : UserPhoto -> String
photoToUrl (UserPhoto maybeUrl) =
    case maybeUrl of
        Nothing ->
            "https://static.productionready.io/images/smiley-cyrus.jpg"

        Just url ->
            url

module Data.User.Username exposing (Username, decoder, encode, eq, parser, toHtml, toString)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import StringEq as String
import Url.Parser


type Username
    = Username String


eq : Username -> Username -> Bool
eq (Username a) (Username b) =
    String.eq a b


toString : Username -> String
toString (Username username) =
    username


parser : Url.Parser.Parser (Username -> a) a
parser =
    Url.Parser.custom "USERNAME" (Just << Username)


decoder : Decoder Username
decoder =
    Decode.map Username Decode.string


encode : Username -> Value
encode (Username username) =
    Encode.string username


toHtml : Username -> Html msg
toHtml (Username username) =
    Html.text username

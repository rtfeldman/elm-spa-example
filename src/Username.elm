module Username exposing (Username, decoder, encode, toHtml, toString, urlParser)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser



-- TYPES


type Username = Username String



-- CREATE


decoder = Decode.map Username Decode.string



-- TRANSFORM


encode (Username username) = Encode.string username


toString (Username username) = username


urlParser = Url.Parser.custom "USERNAME" (\str -> Just (Username str))


toHtml (Username username) = Html.text username

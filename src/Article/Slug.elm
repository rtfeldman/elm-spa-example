module Article.Slug exposing (Slug, decoder, toString, urlParser)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser exposing (Parser)



-- TYPES


type Slug
    = Slug String



-- CREATE


urlParser =
    Url.Parser.custom "SLUG" (\str -> Just (Slug str))


decoder =
    Decode.map Slug Decode.string



-- TRANSFORM


toString (Slug str) =
    str

module Data.Article.Slug exposing (Slug, decoder, eq, parser, toString)

import Json.Decode as Decode exposing (Decoder)
import StringEq as String
import Url.Parser


type Slug
    = Slug String


eq : Slug -> Slug -> Bool
eq (Slug a) (Slug b) =
    String.eq a b


parser : Url.Parser.Parser (Slug -> a) a
parser =
    Url.Parser.custom "SLUG" (Just << Slug)


decoder : Decoder Slug
decoder =
    Decode.map Slug Decode.string


toString : Slug -> String
toString (Slug slug) =
    slug

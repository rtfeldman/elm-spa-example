module Data.Article.Tag exposing (Tag, decoder, eq, toString)

import Json.Decode as Decode exposing (Decoder)
import StringEq as String


type Tag
    = Tag String


eq : Tag -> Tag -> Bool
eq (Tag a) (Tag b) =
    String.eq a b


toString : Tag -> String
toString (Tag slug) =
    slug


decoder : Decoder Tag
decoder =
    Decode.map Tag Decode.string

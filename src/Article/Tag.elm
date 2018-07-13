module Article.Tag exposing (Tag, list, toString)

import Http
import Json.Decode as Decode exposing (Decoder)
import Util exposing (apiUrl)



-- TYPES


type Tag
    = Tag String



-- CONVERT


toString : Tag -> String
toString (Tag slug) =
    slug



-- LIST


list : Http.Request (List Tag)
list =
    Decode.field "tags" (Decode.list decoder)
        |> Http.get (apiUrl "/tags")



-- SERIALIZATION


decoder : Decoder Tag
decoder =
    Decode.map Tag Decode.string

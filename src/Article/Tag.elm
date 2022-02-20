module Article.Tag exposing (Tag, list, toString)

-- TYPES


type Tag
    = Tag String



-- TRANSFORM


toString : Tag -> String
toString (Tag slug) =
    slug



-- LIST


list : Http.Request (List Tag)
list =
    Decode.field "tags" (Decode.list decoder)
        |> Api.get Endpoint.tags Nothing



-- SERIALIZATION


decoder : Decoder Tag
decoder =
    Decode.map Tag Decode.string
